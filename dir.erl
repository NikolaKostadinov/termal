-module(dir).
-compile(export_all).

inv(up) -> down;
inv(down) -> up;
inv(left) -> right;
inv(right) -> left;
inv(_) -> none.

clock(up) -> right;
clock(right) -> down;
clock(down) -> left;
clock(left) -> up;
clock(_) -> none.

aclock(Dir) -> clock(clock(clock(Dir))).
