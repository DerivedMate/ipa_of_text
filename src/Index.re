[%raw "require('isomorphic-fetch')"];
open Node;
open Lets;
open Helpers.Ops;
open Js.Promise;
let path_in = Process.argv[2];
let file_in = Fs.readFileSync(path_in, `utf8);
let rex = [%bs.re "/\\n+/g"];
let re_terminator = [%bs.re "/[\s,\.;\!\?\"]/"];

let url_or_word = word => {
  let parsed =
    Js.String2.(replaceByRe(word, [%bs.re "/\s+/g"], "-") |> toLowerCase);
  {j|https://dictionary.cambridge.org/dictionary/english/$parsed|j};
};

// ipa_of_word("cat");
let words =
  Js.String2.splitByRe(file_in, [%bs.re "/\s+/g"])
  ->Belt.Array.map(w =>
      switch (w) {
      | Some(w) => w
      | None => ""
      }
    );

let chunks =
  Js.String2.splitByRe(file_in, [%bs.re "/\s{0}/g"])
  ->Belt.Array.concat([|None|])
  ->Belt.Array.reduce(
      [|("", "")|],
      (back, w) => {
        let (acc, _) = back[0];
        let runes = Belt.Array.sliceToEnd(back, 1);
        let is_term_acc = re_terminator @? acc;
        switch (w) {
        | Some(c) =>
          let is_term_c = re_terminator @? c;
          if (is_term_c && !is_term_acc) {
            let (w, suffix) = Grammar.grammarify(acc);
            Belt.Array.concatMany([|
              [|("", "")|],
              runes,
              [|(w, suffix), (c, "")|],
            |]);
          } else {
            (acc ++ c, "") @|> runes;
          };
        | _ =>
          if (is_term_acc) {
            runes @|< (acc, "");
          } else {
            let (w, suffix) = Grammar.grammarify(acc);
            runes @|< (w, suffix);
          }
        };
      },
    )
  ->Belt.Array.keepMap(((c, suff)) =>
      if (c == "") {
        None;
      } else {
        Some((c, suff));
      }
    );

let (tokens, structure) =
  chunks->Belt.Array.reduce(
    ([||], [||]),
    ((tokens, structure), (w, suffix)) => {
      let w = Js.String2.toLowerCase(w);
      if (w == "") {
        (tokens, structure);
      } else if (!tokens->Belt.Array.some(t => w == t)) {
        (tokens @|< w, structure @|< (w, suffix));
      } else {
        (tokens, structure @|< (w, suffix));
      };
    },
  );
Js.Console.log(tokens);
let tokens =
  tokens
  ->Belt.Array.map(w =>
      if (re_terminator
          @? w
          || w == " "
          || Js.Re.test_([%bs.re "/^\d+$/g"], w)) {
        resolve((w, w));
      } else {
        Fetcher.fetch(w) |> then_(t => (t, w) |> resolve);
      }
    )
  ->all
  |> then_(tokens => {
       structure->Belt.Array.map(((token, suffix)) => {
         let entry = Belt.Array.getBy(tokens, ((_, t)) => t == token);
         switch (entry) {
         | Some((ipa, _)) => ipa ++ suffix
         | None => token
         };
       })
       |> resolve
     })
  |> then_(arr => arr->Belt.Array.reduce("", (++))->Js.Console.log |> resolve);
Js.Console.log(structure);
/*
 Js.Console.logMany(
   words->Belt.Array.map(x => Js.String2.concat(x, "<END>\n")),
 );*/