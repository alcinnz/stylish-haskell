Currently Stylish Haskell lacks support for psuedo-classes/elements, which is a significant shortcoming. So what would it take?

To start the following psuedoclasses need to be defined by the caller (mostly in implementing forms), either before or after the CSS is evaluated:

* active
* checked
* default
* defined -- selects builtin elements or "custom elements".
* disabled
* empty -- since I do not directly reference children, just parents.
* enabled
* focus
* focus-within
* hover
* indeterminate
* in-range
* invalid
* optional
* out-of-range
* read-only
* read-write
* required
* target
* valid
* visited

The pseudoelements meanwhile need to be applied after selection:
* before
* after
* cue
* first-letter
* first-line
* selection

:link, :lang(), & :root/:scope can compile down to existing selectorFuncs. 

Furthermore there's those psuedoclasses which tests the position of the element in the list.
* first-child
* first-of-type
* last-child
* last-of-type
* only-child
* only-of-type
* :nth-child()
* :nth-last-child()
* :nth-last-of-type()
* :nth-of-type()
:first-child, :first-of-type, :nth-child(), :nth-of-type() can easily be tested by traversing prev pointers.
But for only/last psuedoclasses I'd need to store additional counts.

Furthermore there's functional psuedoclasses which takes additional arguments:
* :not(selector)
* :where()/:is()
I need to compile these down myself.

I'd be tempted to output a structure mapping psuedoclasses to style objects,
after having compiled some of those as part of the selector itself.
Though the issue with that is that it prevents popular solutions for creating JavaScript-free webpages.

I guess I'd add a psuedoclass field to elements, and there's already a hook
where embedders can decide how they want to deal with it.

Sorry this document is a bit rambly!
