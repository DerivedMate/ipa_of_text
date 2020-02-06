open Js.Re;
open Js.String2;
open Helpers.Ops;
let re_vowelNcons = [%bs.re
  "/(?<vs>a|e|i|o|u|y)([b-d]|[f-h]|[j-t][uwxz])$/gi"
];
let re_vowel = [%bs.re "/[aeiouy]/gi"];
let re_diphthong = [%bs.re "/(a|e|i|o|u|y){2}/gi"];
let re_last_e = [%bs.re "/e$/i"];
let re_last_s_or_ss = [%bs.re "/ss?$/i"];
let re_last_letter_double = [%bs.re "/(?<l>\w)(\k<l>)$/i"];
let re_last_ed = [%bs.re "/ed$/i"];
let re_last_ing = [%bs.re "/ing$/i"];
/* Voiceless consonants excluding "t" */
let re_last_voiced_cons = [%bs.re "/(ch|f|k|p|sh?|th)$/i"];
let re_cons_i = [%bs.re "/([b-d]|[f-h]|[j-t][uwxz])i$/i"];
let re_last_sibilant = [%bs.re "/(ss?h?|ch?|zh?|j|x)/i"];
let re_last_voiceless = [%bs.re "/(ph?|k|t|f|gh)$/i"];
let re_last_voiced = [%bs.re "/(b|d|g|l|m|ng?|r|v)$/i"];
let re_apostrophy = [%bs.re "/'(\w*)$/i"];

type tense =
  | D
  | Re
  | Ve
  | Ll;

type possessive =
  | VoicelessCons
  | VoicedCons
  | Sibilant;

type suffix =
  | Ing
  | Ed(string)
  | Possesive(string)
  | Aux(tense)
  | No;

let is_verb_e = word => {
  Static__Verbs.verbs_e->Belt.Array.some(
    test_(Js.Re.fromStringWithFlags(word ++ "e?$", ~flags="i")),
  );
};

let is_word_e = word => {
  Static__Words.words_e->Belt.Array.some(
    test_(Js.Re.fromStringWithFlags(word ++ "e?$", ~flags="i")),
  );
};

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
  if (re_last_e @? word) {
    vowel_count - diphthongs_count - 1;
  } else {
    vowel_count - diphthongs_count;
  };
};
/// Checks whether a given verb is its present participle.
/// Returns (basic_form, )
let is_ing = word =>
  if (re_last_ing @? word) {
    let word_stem = ref(replaceByRe(word, re_last_ing, ""));

    if (test_(re_last_letter_double, word_stem^)) {
      let letter = (word_stem^)->Js.String2.charAt(length(word_stem^) - 1);
      word_stem := replaceByRe(word_stem^, re_last_letter_double, letter);
    } else if (is_verb_e(word_stem^)) {
      word_stem := concat(word_stem^, "e");
    };
    (word_stem^, Ing);
  } else {
    (word, No);
  };

let is_ed = word =>
  if (re_last_ed @? word) {
    let word_stem = replaceByRe(word, re_last_ed, "");

    if (is_verb_e(word_stem ++ "e")) {
      (word_stem ++ "e", Ed(word_stem));
    } else if (re_cons_i @? word_stem) {
      let word_stem = slice(word_stem, ~from=0, ~to_=length(word_stem) - 2);
      (word_stem ++ "y", Ed(word_stem));
    } else if (re_last_letter_double @? word_stem) {
      let letter = word_stem->Js.String2.charAt(length(word_stem) - 1);
      let word_stem = replaceByRe(word_stem, re_last_letter_double, letter);
      (word_stem, Ed(word_stem));
    } else {
      (word_stem, Ed(word_stem));
    };
  } else {
    (word, No);
  };

let is_plural = word => {
  let len = word->length;
  let last_two = word->sliceToEnd(~from=len - 2);
  if ([|"is", "as"|]->Belt.Array.some(a => a == word)) {
    (word, No);
  } else if (last_two == "es") {
    let word_wo_s = word->slice(~from=0, ~to_=len - 1);
    Js.Console.log({j|[is_plural] $last_two $word_wo_s|j});
    if (is_word_e(word_wo_s)) {
      (word, Possesive({js|z|js}));
    } else {
      (word, Possesive({js|ɪz|js}));
    };
  } else if (last_two->charAt(1) == "s" && last_two->charAt(0) != "s") {
    if (re_last_sibilant @? word) {
      (word, Possesive({js|ɪz|js}));
    } else if (re_last_voiceless @? word) {
      (word, Possesive({js|s|js}));
    } else {
      (word, Possesive({js|z|js}));
    };
  } else {
    (word, No);
  };
};

let is_possesive = (word_stem, contraction) => {
  let word_stem = ref(word_stem);
  let contr = ref(contraction);
  // Handle plurla possesives
  if (re_last_s_or_ss @? word_stem^ && contraction == "") {
    let len = length(word_stem^);
    word_stem := slice(word_stem^, ~from=0, ~to_=len - 2);
    contr := slice(word_stem^, ~from=len - 2, ~to_=len - 1);
  };

  is_plural(word_stem^);
  // if(test_(re_last_sibilant, word_stem^))
  // (word_stem, Possesive(contraction));
};

let is_apostrophe = word =>
  if (re_apostrophy @? word) {
    let word_stem = replaceByRe(word, re_apostrophy, "");
    let contraction =
      exec_(re_apostrophy, word)->Belt.Option.getExn->captures[1]
      ->Js.Nullable.toOption;

    switch (contraction) {
    | Some("d") => (word_stem, Aux(D))
    | Some("ve") => (word_stem, Aux(Ve))
    | Some("re") => (word_stem, Aux(Re))
    | Some("ll") => (word_stem, Aux(Ll))
    | Some("s") => is_possesive(word_stem, "s")
    | None => is_plural(word_stem)
    };
  } else {
    (word, No);
  };

let ipa_of_suffix = suff =>
  switch (suff) {
  | No => ""
  | Ing => {j|ɪŋ|j}
  | Ed(word) =>
    if (re_last_voiced_cons @? word) {
      {j|t|j};
    } else {
      {j|d|j};
    }
  | Aux(D) => {j|əd|j}
  | Aux(Ve) => {j|v|j}
  | Aux(Re) => {j|ɚ|j}
  | Aux(Ll) => {j|əl|j}
  | Possesive(c) => c
  };

let grammarify = word => {
  let (wrd, suff) = (ref(word), ref(No));
  for (i in 0 to 3) {
    let (w, suff_) =
      switch (i) {
      | 0 => is_ing(word)
      | 1 => is_ed(word)
      | 2 => is_plural(word)
      | 3 => is_apostrophe(word)
      | _ => ("", No)
      };
    if (suff_ != No) {
      suff := suff_;
      wrd := w;
    };
  };
  let ip = ipa_of_suffix(suff^);
  let w = wrd^;
  // Js.Console.log({j|$w : $ip|j});
  (w, ip);
  // (w, )
};