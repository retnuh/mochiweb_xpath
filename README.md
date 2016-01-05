[XPath 1.0](http://www.w3.org/TR/xpath/) interpreter for
[mochiweb's html tree](https://github.com/mochi/mochiweb/blob/master/src/mochiweb_html.erl)
See src/test.erl for examples

Build
-----

    make
    make test

API
---

    mochiweb_xpath:execute/2
	mochiweb_xpath:execute/3
    mochiweb_xpath:compile_xpath/1

Usage
-----

    Tree = mochiweb_html:parse(<<"some html">>),
    Results = mochiweb_xpath:execute("/some/xpath", Tree).

Note
----

`mochiweb_xpath_parser:compile_xpath/1` utilize two undocumented functions
in  xmerl (`xmerl_xpath_parse:parse/1` and `xmerl_xpath_scan:tokens/1`).
These functions could change between OTP versions.. I'm using R12B2

Xpath coverage
--------------

### Implemented axes

* self
* child
* descendant-or-self
* descendant
* parent (only for elements, not `text()/parent::` or `attribute::*/parent::`)
* following-sibling (same as parent)
* preceding-sibling (same as parent)
* attribute
 

### Implemented functions

* last
* position
* count
* concat
* ends-with
* name
* starts-with
* contains
* substring
* sum
* string-length
* not
 

### Implemented abbreviated syntax

* indexed access (e.g. `div[last() - 1]`)


### TODO axes

* ancestor
* ancestor-or-self
* following
* namespace
* preceding

### TODO functions

* id
* local-name
* namespace-uri
* string
* substring-before
* substring-after
* normalize-space
* translate
* boolean
* true
* false
* lang
* number
* sum
* floor
* ceiling
* round

### TODO:
port `match_*` from xmerl_xpath for better axes support
