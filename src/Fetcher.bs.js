// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("bs-platform/lib/js/curry.js");
var Cheerio = require("cheerio");
var Caml_option = require("bs-platform/lib/js/caml_option.js");
var Querystring = require("querystring");
var Cheerio$BsCheerio = require("bs-cheerio/src/Cheerio.bs.js");

function ipa_of_word(word, url_of_word, selector_ipa_node) {
  if (word === "th") {
    return Promise.resolve("θ");
  } else {
    return fetch(Curry._1(url_of_word, word)).then((function (prim) {
                      return prim.text();
                    })).then((function (html) {
                    return Promise.resolve(Caml_option.nullable_to_opt(Curry._2(Cheerio$BsCheerio.select, Cheerio.load(html), selector_ipa_node).first().text()));
                  })).then((function (w) {
                  return Promise.resolve(w !== undefined ? w.trim().replace((/\//gi), "") : (console.log("no match for \"" + (String(word) + "\"")), word));
                }));
  }
}

function url_of_word(word) {
  var parsed = Querystring.escape(word.replace((/\s+/g), "-").toLowerCase());
  var url = "https://www.oxfordlearnersdictionaries.com/definition/english/" + (String(parsed) + "");
  console.log("" + (String(url) + (" : " + (String(word) + ""))));
  return url;
}

var Oxford = {
  url_of_word: url_of_word,
  selector_ipa_node: "span.phon"
};

function url_of_word$1(word) {
  var parsed = Querystring.escape(word.replace((/\s+/g), "-").toLowerCase());
  return "https://dictionary.cambridge.org/dictionary/english/" + (String(parsed) + "");
}

var selector_ipa_node = "span.ipa";

var Cambridge = {
  url_of_word: url_of_word$1,
  selector_ipa_node: selector_ipa_node
};

function $$fetch$1(word) {
  if ((/\w+/gi).test(word)) {
    return ipa_of_word(word, url_of_word$1, selector_ipa_node);
  } else {
    return Promise.resolve(word);
  }
}

exports.ipa_of_word = ipa_of_word;
exports.Oxford = Oxford;
exports.Cambridge = Cambridge;
exports.$$fetch = $$fetch$1;
/* cheerio Not a pure module */