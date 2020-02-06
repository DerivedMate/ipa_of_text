open Js.Promise;
open BsCheerio;

module type Site = {
  let url_of_word: string => string;
  let selector_ipa_node: string;
};

let ipa_of_word = (word, url_of_word, selector_ipa_node) => {
  switch (word) {
  | "th" => resolve({j|θ|j})
  | _ =>
    Fetch.fetch(url_of_word(word))
    |> then_(Fetch.Response.text)
    |> then_(html => {
         Cheerio.load(html)
         ->Cheerio.select(selector_ipa_node)
         ->Element.first
         ->Element.text
         ->Js.Nullable.toOption
         |> resolve
       })
    |> then_(w => {
         (
           switch (w) {
           | Some(wd) =>
             wd
             ->Js.String2.trim
             ->Js.String2.replaceByRe([%bs.re "/\//gi"], "")
           | None =>
             Js.Console.log({j|no match for "$word"|j});
             word;
           }
         )
         |> resolve
       })
  };
};

module Oxford: Site = {
  let url_of_word = word => {
    let parsed =
      Js.String2.(replaceByRe(word, [%bs.re "/\s+/g"], "-") |> toLowerCase)
      ->Helpers.QueryString.escape;
    // let url = {j|https://www.oxfordlearnersdictionaries.com/definition/english/$parsed|j};
    let url = {j|https://www.oxfordlearnersdictionaries.com/definition/english/$parsed|j};
    Js.Console.log({j|$url : $word|j});
    url;
  };
  let selector_ipa_node = "span.phon";
};

module Cambridge: Site = {
  let url_of_word = word => {
    let parsed =
      Js.String2.(replaceByRe(word, [%bs.re "/\s+/g"], "-") |> toLowerCase)
      ->Helpers.QueryString.escape;
    {j|https://dictionary.cambridge.org/dictionary/english/$parsed|j};
  };

  let selector_ipa_node = "span.ipa";
};

let fetch = word => {
  module Provider = Cambridge;
  if (!Js.Re.test_([%bs.re "/\w+/gi"], word)) {
    resolve(word);
  } else {
    ipa_of_word(word, Provider.url_of_word, Provider.selector_ipa_node);
  };
};