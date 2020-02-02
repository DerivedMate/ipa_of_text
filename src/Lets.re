module Async {
  let let_ = (pr, cb) => Js.Promise.then_(cb, pr);
}; 