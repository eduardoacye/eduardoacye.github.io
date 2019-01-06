---
layout: post
title: First blog post
date: Sat 05 Jan 2019 06:32:54 PM MST
tags: Misc
---

# Heading

## Sub-heading

### Sub-sub-heading

#### Sub-sub-sub-heading

##### Sub-sub-sub-sub-heading

###### Sub-sub-sub-sub-sub-heading

_Lorem_ __ipsum__ `dolor` ~~sit~~ amet, consectetur adipiscing elit. Morbi quis quam pulvinar, tristique quam quis, porta purus. Fusce ipsum sem, elementum sit amet fermentum vitae, fringilla ac risus. Phasellus velit odio, vulputate ac mi at, accumsan consectetur risus. Suspendisse ornare ex turpis, sit amet gravida diam congue non. Duis sit amet nunc eu justo blandit commodo. Aenean blandit luctus efficitur. Nam id nibh vestibulum lacus consectetur elementum vel eget ipsum. Suspendisse potenti. Nulla id lorem at augue commodo tempor quis in nibh. Mauris bibendum malesuada iaculis. Maecenas placerat condimentum lacinia. Aenean commodo elementum dolor, nec ultrices libero tincidunt ut.[^1]

[^1]: classic placeholder text

Suspendisse potenti. Vestibulum facilisis orci eu urna efficitur ultrices. Curabitur nec pharetra ex, sed fringilla quam. Curabitur in faucibus nulla. Nam sit amet gravida arcu. Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos. Etiam vel porttitor tortor. Maecenas lobortis, velit vulputate placerat lacinia, nisl mauris maximus arcu, quis molestie metus elit vitae odio. Vestibulum eget tortor faucibus, malesuada mauris sed, tempus libero. Aliquam placerat sodales aliquet. Pellentesque habitant morbi tristique senectus et netus et malesuada fames ac turpis egestas. Quisque efficitur orci sit amet nunc ultrices, sit amet laoreet tellus aliquam. Sed odio odio, scelerisque suscipit varius eu, tincidunt at odio. Quisque volutpat sapien nec est vehicula, in congue magna tincidunt. In hac habitasse platea dictumst. Aliquam vel nisl ut velit mattis aliquam.

*[ipsum]: Some abbreviation explanation

Aliquam eu pretium leo, ac semper ipsum. Integer ligula tellus, accumsan sed fermentum mollis, ultrices sed sapien. Nullam convallis consectetur metus, ut accumsan urna lobortis eget. Suspendisse egestas pulvinar justo non fringilla. Etiam tortor sem, vestibulum quis porttitor a, cursus imperdiet urna. Sed mattis molestie felis vulputate mattis. Vestibulum a fringilla metus, quis egestas sem.
{:.side-note}

Duis pellentesque turpis id porttitor tempus. Maecenas interdum fringilla pulvinar. Cras malesuada mi et ex aliquet efficitur. Nunc nec condimentum dui. Nam interdum eros ut ultrices facilisis. Curabitur mollis tincidunt condimentum. Quisque urna purus, vestibulum id ligula at, ornare dignissim mauris.

Morbi commodo venenatis ornare. Sed at magna vel nibh molestie feugiat vitae porttitor quam. In hac habitasse platea dictumst. Donec sem mi, rutrum id quam nec, maximus egestas felis. Sed tempus fringilla orci, in faucibus dui pulvinar ut. Ut lobortis est in laoreet fermentum. Aenean venenatis congue ligula ac laoreet. Suspendisse eros arcu, pellentesque egestas pretium ut, vestibulum eget enim. Proin dignissim nec magna et tempus. Sed accumsan urna laoreet nisl viverra, eu congue libero sollicitudin.

> Curabitur quis nisl at ex finibus aliquet. Pellentesque at lacus dapibus neque congue vestibulum. Curabitur sagittis, felis in rhoncus dapibus, elit purus tincidunt orci, vitae porttitor mi tortor ac augue. Fusce vitae nisi leo. Sed turpis orci, feugiat sed eleifend nec, aliquam id felis. Nam libero lorem, egestas at molestie venenatis, rutrum et turpis. Nulla aliquet sit amet velit ut bibendum. Sed ultricies lacinia tempor. Donec pharetra et magna sit amet consectetur. Quisque et hendrerit libero. Curabitur sem arcu, imperdiet eu vehicula a, tincidunt at libero. Phasellus tincidunt, diam at eleifend fringilla, lacus arcu vestibulum lorem, non condimentum erat magna eget quam. Donec eget dignissim augue. Proin condimentum lacus libero, id vestibulum diam volutpat nec. Etiam nec ipsum dui.

# Common Lisp code

```common-lisp
(let ((x 1266778)
      (y 458))
  (multiple-value-bind (quotient remainder)
      (truncate x y)
    (format nil "~A divided by ~A is ~A remainder ~A" x y quotient remainder)))
```

# Scheme code

```scheme
(define (find-first func lst)
  (call-with-current-continuation
   (lambda (return-immediately)
     (for-each (lambda (x)
                 (if (func x)
                     (return-immediately x)))
               lst)
     #f)))
```

# Clojure code

```clojure
(map (fn [form]
       (case form
         1 'one
         + 'plus))
     (quote (+ 1 1)))
```

# C code

```c
#include <stdio.h>

int main(void)
{
    printf("hello, world\n");
}
```

# Math

\\[2x+3\\]

\\[ \\sum_{i=0}^{n} \\frac{i^{2}}{\\pi} \\]

This is an example of inline math: \\( E = mc^{2} \\).
