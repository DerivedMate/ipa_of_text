open Js.Re;
open Js.String2;
let re_vowelNcons = [%bs.re
  "/(?<vs>a|e|i|o|u|y)([b-d]|[f-h]|[j-t][uwxz])$/gi"
];
let re_vowel = [%bs.re "/[aeiouy]/gi"];
let re_diphthong = [%bs.re "/(a|e|i|o|u|y){2}/gi"];
let re_last_e = [%bs.re "/e$/i"];
let re_last_letter_double = [%bs.re "/(?<l>\w)(\k<l>)$/i"];

let count_syllables = word => {
  let vowel_count =
    switch (exec_(re_vowel, word)) {
    | Some(vs) => vs |> captures |> Belt.Array.length
    | None => 0
    };
  let diphthongs_count =
    switch (exec_(re_diphthong, word)) {
    | Some(vs) => vs |> captures |> Belt.Array.length
    | None => 0
    };
  if (test_(re_last_e, word)) {
    vowel_count - diphthongs_count - 1;
  } else {
    vowel_count - diphthongs_count;
  };
};
/// Checks whether a given verb is its present participle.
/// Returns (basic_form, )
let is_ing = word =>
  if (test_([%bs.re "/ing$/i"], word)) {
    let word_stem = ref(replaceByRe(word, [%bs.re "/ing$/i"], ""));
    let re_is_word_stem = Js.Re.fromStringWithFlags(word_stem^, ~flags="i");

    if (test_(re_last_letter_double, word_stem^)) {
      let letter = (word_stem^)->Js.String2.charAt(length(word_stem^) - 1);
      word_stem := replaceByRe(word_stem^, re_last_letter_double, letter);
    } else if (Static__Verbs.verbs_e->Belt.Array.some(test_(re_is_word_stem))) {
      word_stem := concat(word_stem^, "e");
    };
    (word_stem^, Some("ing"));
  } else {
    (word, None);
  };
let grammarify = word => {
  let (w, suffix) =
    switch (is_ing(word)) {
    | (w, Some("ing")) => (w, {j|ɪŋ|j})
    | (w, None) => (w, "")
    };
  (w, suffix);
};