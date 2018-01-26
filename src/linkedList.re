type linkedList('a) =
  | Empty
  | Node('a, linkedList('a));

let rec addEnd = (head, elem) =>
  switch head {
  | Empty => Node(elem, Empty)
  | Node(v, tail) => Node(v, addEnd(tail, elem))
  };

let add = (head, elem) => {
  switch head {
  | Empty => Node(elem, Empty)
  | _ => Node(elem, head)
  }
};

let head = fun | Empty => Empty
                | Node(v, _) => Node(v, Empty);

let tail = fun | Empty => Empty
                | Node(_, tail) => tail;

let rec reverse = fun | Empty => Empty
                      | Node(v, t) => addEnd(reverse(t), v);

let rec length = fun | Empty => 0
                     | Node(_, t) => length(t) + 1;

let rec concat = (a, b) => switch a {
| Empty => b
| Node(v, Empty) => Node(v, b)
| Node(v, tail) => Node(v, concat(tail, b))
};

let rec findIndex = (~count=0, head, item) => switch head {
| Empty => -1
| Node(v, tail) =>
  if (item == v) {
    count
  } else {
    findIndex(~count=count + 1, tail, item)
  }
};

let rec itemExists = (head, item) => switch head {
| Empty => false
| Node(v, tail) => if (v == item) {
    true 
  } else {
    itemExists(tail, item)
  }
};

let list1 = Empty;

let list2 = add(list1, "hello");

let list3 = add(list2, "world");

let list4 = Node("1", Node("2", Node("3", Empty)));

Js.log(findIndex(list4, "1"));
Js.log(findIndex(list4, "2"));
Js.log(findIndex(list4, "3"));
Js.log(findIndex(list4, "234"));