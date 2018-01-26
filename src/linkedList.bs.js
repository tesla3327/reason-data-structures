// Generated by BUCKLESCRIPT VERSION 2.1.0, PLEASE EDIT WITH CARE
'use strict';

var Caml_obj = require("bs-platform/lib/js/caml_obj.js");

function addEnd(head, elem) {
  if (head) {
    return /* Node */[
            head[0],
            addEnd(head[1], elem)
          ];
  } else {
    return /* Node */[
            elem,
            /* Empty */0
          ];
  }
}

function add(head, elem) {
  if (head) {
    return /* Node */[
            elem,
            head
          ];
  } else {
    return /* Node */[
            elem,
            /* Empty */0
          ];
  }
}

function head(param) {
  if (param) {
    return /* Node */[
            param[0],
            /* Empty */0
          ];
  } else {
    return /* Empty */0;
  }
}

function tail(param) {
  if (param) {
    return param[1];
  } else {
    return /* Empty */0;
  }
}

function reverse(param) {
  if (param) {
    return addEnd(reverse(param[1]), param[0]);
  } else {
    return /* Empty */0;
  }
}

function length(param) {
  if (param) {
    return length(param[1]) + 1 | 0;
  } else {
    return 0;
  }
}

function concat(a, b) {
  if (a) {
    var tail = a[1];
    var v = a[0];
    if (tail) {
      return /* Node */[
              v,
              concat(tail, b)
            ];
    } else {
      return /* Node */[
              v,
              b
            ];
    }
  } else {
    return b;
  }
}

function findIndex(_$staropt$star, _head, item) {
  while(true) {
    var head = _head;
    var $staropt$star = _$staropt$star;
    var count = $staropt$star ? $staropt$star[0] : 0;
    if (head) {
      if (Caml_obj.caml_equal(item, head[0])) {
        return count;
      } else {
        _head = head[1];
        _$staropt$star = /* Some */[count + 1 | 0];
        continue ;
        
      }
    } else {
      return -1;
    }
  };
}

function itemExists(_head, item) {
  while(true) {
    var head = _head;
    if (head) {
      if (Caml_obj.caml_equal(head[0], item)) {
        return /* true */1;
      } else {
        _head = head[1];
        continue ;
        
      }
    } else {
      return /* false */0;
    }
  };
}

var list2 = add(/* Empty */0, "hello");

var list3 = add(list2, "world");

var list4 = /* Node */[
  "1",
  /* Node */[
    "2",
    /* Node */[
      "3",
      /* Empty */0
    ]
  ]
];

console.log(findIndex(/* None */0, list4, "1"));

console.log(findIndex(/* None */0, list4, "2"));

console.log(findIndex(/* None */0, list4, "3"));

console.log(findIndex(/* None */0, list4, "234"));

var list1 = /* Empty */0;

exports.addEnd     = addEnd;
exports.add        = add;
exports.head       = head;
exports.tail       = tail;
exports.reverse    = reverse;
exports.length     = length;
exports.concat     = concat;
exports.findIndex  = findIndex;
exports.itemExists = itemExists;
exports.list1      = list1;
exports.list2      = list2;
exports.list3      = list3;
exports.list4      = list4;
/* list2 Not a pure module */