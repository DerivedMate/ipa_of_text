let take_while = (arr, check, ~i0=0) => {
  let i = ref(0);
  let continue = ref(true);
  let len = Belt.Array.length(arr);

  while (i^ < len && continue^) {
    continue :=
      (
        switch (Belt.Array.get(arr, i^ + i0)) {
        | Some(a) => check(a)
        | None => false
        }
      );
  };

  (Belt.Array.slice(arr, ~offset=i0, ~len=i^), i^ + i0);
};

module Ops = {
  let (@<) = Belt.List.add;
  let (@|<) = (arr, el) => Belt.Array.concat(arr, [|el|]);
  let (@|>) = (el, arr) => Belt.Array.concat([|el|], arr);
  let (@?) = Js.Re.test_;
};

module QueryString = {
  [@bs.module "querystring"] external escape: string => string = "escape";
};