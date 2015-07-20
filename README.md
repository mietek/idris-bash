_idris-bash_
============

GNU _bash_ backend for Idris.

Based on Edwin Brady’s [PHP backend](https://github.com/edwinb/idris-php), and my [toy JavaScript backend](https://github.com/mietek/idris-js).

Barely functional.  Super slow.


Example
-------

    $ idris pythag.idr --codegen bash -o pythag.sh
    $ time bash pythag.sh
    [(3, (4, 5)), (6, (8, 10)), (5, (12, 13)), (9, (12, 15))]

    real    0m9.965s
    user    0m9.938s
    sys     0m0.016s

Input: [`pythag.idr`](pythag.idr)

Output: [`pythag.sh`](https://gist.github.com/mietek/7fbb604d186042f613d3)


About
-----

Made by [Miëtek Bak](https://mietek.io/).  Published under the [MIT X11 license](LICENSE.md).
