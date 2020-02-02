[%raw "require('isomorphic-fetch')"];
open Node;
open Js.Promise;
let path_in = Process.argv[2];
let file_in = Fs.readFileSync(path_in, `utf8);
let rex = [%bs.re "/\\n+/g"];

let url_or_word = word => {
  let parsed =
    Js.String2.(replaceByRe(word, [%bs.re "/\s+/g"], "-") |> toLowerCase);
  {j|https://dictionary.cambridge.org/dictionary/english/$parsed|j};
};

let ipa_of_word = word => {
  // Get the IPA of the word
  Fetch.fetch(url_or_word(word))
  |> then_(Fetch.Response.text)
  |> then_(html => {
       let rex = [%bs.re
         "/<span class=\"ipa dipa lpr-2 lpl-1\">(\S+)<\/span>\/<\/span>/g"
       ];
       (
         switch (Js.Re.exec_(rex, html)) {
         | Some(res) => Js.Re.captures(res)
         | None => [||]
         }
       )
       |> resolve;
     })
  |> then_(ms =>
       ms
       ->Belt.Array.map(x =>
           switch (Js.Nullable.toOption(x)) {
           | Some(x) => x
           | None => ""
           }
         )
       ->Belt.Array.get(1)
       |> resolve
     )
  |> then_((w: option(string)) =>
       (
         switch (w) {
         | Some(wd) => wd |> Js.String2.trim
         | None => word
         }
       )
       |> resolve
     );
      // Parse the grammar
};

// ipa_of_word("cat");

Js.String2.splitByRe(file_in, [%bs.re "/\s+/g"])
->Belt.Array.map(x =>
    switch (x) {
    | Some(x) =>
      let (w, suffix) = Grammar.grammarify(x);
      ipa_of_word(w) |> then_(ipa => ipa ++ suffix |> resolve);
    | None => resolve("")
    }
  )
|> all
|> then_(ws => {Js.Console.logMany(ws) |> resolve});

/*
 Js.Console.logMany(
   words->Belt.Array.map(x => Js.String2.concat(x, "<END>\n")),
 );*/