extern crate hyper;
extern crate route_recognizer;

use hyper::header::Headers;
use hyper::method::Method;
use hyper::server::Handler as HyperHandler;
use hyper::server::{Request, Response};
use hyper::status::StatusCode;
use hyper::uri::RequestUri;

use route_recognizer::Router;

pub use route_recognizer::{Match, Params};


#[macro_export]
macro_rules! create_router {
    ( $( $path:expr => $( $method:ident ),+ => $handler:expr ),+, ) => {{
        let mut router = $crate::HyperRouter::new();

        $(
            let mut entry = $crate::RouteEntry::with_handler($handler);
            $(
                entry.add_method($crate::MethodRoutes::$method);
            )+

            router.add_path_entry($path, entry);
        )+

        router
    }};
}


fn method_to_bit(method: &Method) -> u8 {
    use hyper::method::Method::*;
    let exp = match *method {
        Options => 0,
        Get | Head => 1,
        Post => 2,
        Put => 3,
        Delete => 4,
        Connect => 5,
        Patch => 6,
        Trace | Extension(_) => 7,
    };

    1u8 << exp
}

fn method_route_to_bit(method: MethodRoutes) -> u8 {
    use MethodRoutes::*;
    let exp = match method {
        Options => 0,
        Get => 1,
        Post => 2,
        Put => 3,
        Delete => 4,
        Connect => 5,
        Patch => 6,
        Other => 7,
    };

    1u8 << exp
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum MethodRoutes {
    Options,
    Get,
    Post,
    Put,
    Delete,
    Connect,
    Patch,
    Other,
}


#[derive(Debug)]
pub struct RouteEntry<H> {
    accepted_methods: u8,
    handler: H,
}

impl<H> RouteEntry<H> {
    pub fn with_handler(handler: H) -> RouteEntry<H> {
        RouteEntry {
            accepted_methods: 0,
            handler: handler,
        }
    }

    pub fn add_method(&mut self, method: MethodRoutes) {
        self.accepted_methods |= method_route_to_bit(method);
    }

    pub fn add_methods(&mut self, methods: &[MethodRoutes]) {
        for method in methods {
            self.accepted_methods |= method_route_to_bit(*method);
        }
    }

    pub fn add_all_methods(&mut self) {
        self.accepted_methods = !0u8;
    }

    pub fn accepts_method(&self, method: &Method) -> bool {
        (self.accepted_methods & method_to_bit(method)) > 0
    }

    pub fn handler(&self) -> &H {
        &self.handler
    }
}


pub trait RouteHandler: Sync + Send {
    fn handle<'a, 'k>(&'a self, params: Params, req: Request<'a, 'k>, res: Response<'a>);

    fn check_continue(&self, _: Params, _: (&Method, &RequestUri, &Headers)) -> StatusCode {
        StatusCode::Continue
    }
}


impl RouteHandler for Box<RouteHandler> {
    fn handle<'a, 'k>(&'a self, params: Params, req: Request<'a, 'k>, res: Response<'a>) {
        self.as_ref().handle(params, req, res)
    }

    fn check_continue(&self, params: Params, t: (&Method, &RequestUri, &Headers)) -> StatusCode {
        self.as_ref().check_continue(params, t)
    }
}

impl RouteHandler for HyperHandler {
    fn handle<'a, 'k>(&'a self, _: Params, req: Request<'a, 'k>, res: Response<'a>) {
        self.handle(req, res)
    }

    fn check_continue(&self, _: Params, tuple: (&Method, &RequestUri, &Headers)) -> StatusCode {
        self.check_continue(tuple)
    }
}

impl<F> RouteHandler for F
    where F: Fn(Params, Request, Response),
          F: Sync + Send
{
    fn handle<'a, 'k>(&'a self, params: Params, req: Request<'a, 'k>, res: Response<'a>) {
        self(params, req, res)
    }
}

pub struct HyperRouter<H> {
    path_router: Router<RouteEntry<H>>,
    url_router: Router<RouteEntry<H>>,
    authority_router: Router<RouteEntry<H>>,
    server_wide: Option<RouteEntry<H>>,
}

impl<H> HyperRouter<H> {
    pub fn new() -> HyperRouter<H> {
        HyperRouter {
            path_router: Router::new(),
            url_router: Router::new(),
            authority_router: Router::new(),
            server_wide: None,
        }
    }

    pub fn add_path_entry(&mut self, route: &str, entry: RouteEntry<H>) {
        self.path_router.add(route, entry);
    }

    pub fn add_url_entry(&mut self, route: &str, entry: RouteEntry<H>) {
        self.path_router.add(route, entry);
    }

    pub fn add_authority_entry(&mut self, route: &str, entry: RouteEntry<H>) {
        self.path_router.add(route, entry);
    }

    pub fn add_server_wide_entry(&mut self, entry: RouteEntry<H>) {
        self.server_wide = Some(entry);
    }

    pub fn get_match(&self, method: &Method, uri: &RequestUri) -> Option<Match<&RouteEntry<H>>> {
        use hyper::uri::RequestUri::*;

        let m = match *uri {
            AbsolutePath(ref path) => {
                self.path_router
                    .recognize(&path.splitn(2, |c| c == '?' || c == '#').collect::<Vec<&str>>()[0])
                    .ok()
            }
            AbsoluteUri(ref url) => self.url_router.recognize(url.as_str()).ok(),
            Authority(ref authority) => self.authority_router.recognize(&authority).ok(),
            Star => {
                self.server_wide.as_ref().map(|handler| {
                    Match {
                        handler: handler,
                        params: Params::new(),
                    }
                })
            }
        };

        if let Some(mat) = m {
            if mat.handler.accepts_method(method) {
                Some(mat)
            } else {
                None
            }
        } else {
            None
        }
    }
}

impl<H> HyperHandler for HyperRouter<H>
    where H: RouteHandler
{
    fn handle<'a, 'k>(&'a self, req: Request<'a, 'k>, mut res: Response<'a>) {
        if let Some(matches) = self.get_match(&req.method, &req.uri) {
            let Match { handler, params } = matches;
            handler.handler.handle(params, req, res);
        } else {
            *res.status_mut() = StatusCode::NotFound;
        }
    }

    fn check_continue(&self, tuple: (&Method, &RequestUri, &Headers)) -> StatusCode {
        if let Some(matches) = self.get_match(tuple.0, tuple.1) {
            let Match { handler, params } = matches;
            handler.handler.check_continue(params, tuple)
        } else {
            StatusCode::Continue
        }
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    use hyper::method::Method;
    use hyper::uri::RequestUri;

    #[test]
    fn simple_test() {
        let mut entry = RouteEntry::with_handler(0u8);
        entry.add_method(MethodRoutes::Get);
        let mut router = HyperRouter::new();
        router.add_path_entry("/test/", entry);

        let res = router.get_match(&Method::Get,
                                   &RequestUri::AbsolutePath(String::from("/test/")));
        assert_eq!(res.unwrap().handler.handler, 0u8);

        let res = router.get_match(&Method::Get,
                                   &RequestUri::AbsolutePath(String::from("/test2/")));
        assert!(res.is_none());

        let res = router.get_match(&Method::Post,
                                   &RequestUri::AbsolutePath(String::from("/test/")));
        assert!(res.is_none());
    }

    #[test]
    fn multi_method_test() {
        let mut entry = RouteEntry::with_handler(0u8);
        entry.add_method(MethodRoutes::Get);
        entry.add_method(MethodRoutes::Put);
        let mut router = HyperRouter::new();
        router.add_path_entry("/test/", entry);

        let res = router.get_match(&Method::Get,
                                   &RequestUri::AbsolutePath(String::from("/test/")));
        assert_eq!(res.unwrap().handler.handler, 0u8);

        let res = router.get_match(&Method::Put,
                                   &RequestUri::AbsolutePath(String::from("/test/")));
        assert_eq!(res.unwrap().handler.handler, 0u8);

        let res = router.get_match(&Method::Post,
                                   &RequestUri::AbsolutePath(String::from("/test/")));
        assert!(res.is_none());
    }

    #[test]
    fn query_string_test() {
        let mut entry = RouteEntry::with_handler(0u8);
        entry.add_method(MethodRoutes::Get);
        entry.add_method(MethodRoutes::Put);
        let mut router = HyperRouter::new();
        router.add_path_entry("/test/", entry);

        let res = router.get_match(&Method::Get,
                                   &RequestUri::AbsolutePath(String::from("/test/")));
        assert_eq!(res.unwrap().handler.handler, 0u8);

        let res = router.get_match(&Method::Put,
                                   &RequestUri::AbsolutePath(String::from("/test/?test=foo")));
        assert_eq!(res.unwrap().handler.handler, 0u8);

        let res = router.get_match(&Method::Put,
                                   &RequestUri::AbsolutePath(String::from("/test/#test")));
        assert_eq!(res.unwrap().handler.handler, 0u8);
    }

    #[test]
    fn create_router_test() {
        let router = create_router! {
            "/test/" => Get, Put => 0u8,
            "/test2/:id" => Delete => 1u8,
        };

        let res = router.get_match(&Method::Get,
                                   &RequestUri::AbsolutePath(String::from("/test/")));
        assert_eq!(res.unwrap().handler.handler, 0u8);

        let res = router.get_match(&Method::Put,
                                   &RequestUri::AbsolutePath(String::from("/test/")));
        assert_eq!(res.unwrap().handler.handler, 0u8);

        let res = router.get_match(&Method::Delete,
                                   &RequestUri::AbsolutePath(String::from("/test/")));
        assert!(res.is_none());

        let res = router.get_match(&Method::Delete,
                                   &RequestUri::AbsolutePath(String::from("/test2/test")));
        assert_eq!(res.as_ref().unwrap().handler.handler, 1u8);
        assert_eq!(res.unwrap().params["id"], "test");
    }
}
