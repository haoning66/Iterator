(* 
   Ocaml has lists built in but I redifiend it for two reasons, one, fold
   and map are already defined for the builtin lists, and it shows you how
   to declare a relativly simple recursive data type.
   
   If you are familiar with lisp then you should be familar with how these
   lists are implemented. A list is either the empty list, nil in this case,
   or it is the head of a list and the rest of the list. 

   The list [1] is equivalent to Cons (1, Nil).
   The list [1,2] is Cons (1, Cons (2, Nil)).
 *)
type 'a linkedlist = Nil
                   | Cons of 'a * 'a linkedlist;;
(* 
   This tree is implemented as either a EmptyTree, in this case I called it a
   leaf, or as a branch, which contains a value of type a and a list of
   children, which are also trees.
 *)
type 'a tree = Leaf
             | Branch of 'a * 'a tree list;;

(* 
   It isn't obvious why a queue can be implemented as a pair of lists, so I 
   added a enqueue and dequeue functions. The secret is in the rebalance 
   function. When an element is added it is put at the end of the inbox.
   When an item is removed from the queue, it is removed from the end of the 
   outbox. The only time and item is moved from the inbox to the outbox is with
   the rebalancing function that is run when the outbox is empty. The beginning
   of the inbox is the oldest element in the queue when rebalancing is done,
   since the outbox is empty. Rebalancing moves elements from the inbox, to the
   outbox in reverse order. So the first element to be removed from the outbox
   on the next call to dequeue, is the oldest element in the queue. Hence, the
   FIFO invariant is preserved. This implementation of a queue is inspired by
   Chris Okasaki's PhD thesis, "Purely Functional Data Structures".
 *)
type 'a queue = Queue of 'a list * 'a list;;
let rec rebalance (Queue (inbox, outbox)) = match (inbox, outbox) with
  | ([], _) -> Queue (inbox, outbox)
  | (x::xs, ys) -> rebalance (Queue (xs, (x::ys)));;
let enqueue (Queue (inbox, outbox)) a = Queue(a :: inbox, outbox);;
let rec dequeue (Queue (inbox, outbox)) = match (inbox, outbox) with
  | ([], []) -> (Queue ([], []), None)
  | (_::_, []) -> dequeue (rebalance (Queue (inbox, outbox)))
  | (_, y::ys) -> (Queue (inbox, ys), Some y);;

let rec listMap func iter = match iter with
      | Nil -> Nil
      | Cons(h, t) -> Cons(func h, (listMap func t));;

let rec listFold func init listmap = match listmap with
      | Nil -> init
      | Cons(h, t) -> listFold func (func init h) t;;

let rec treeMap func tree = match tree with
      | Leaf -> Leaf
      | Branch(h, t) -> Branch(func h, ((List.map (treeMap func) t)));;

let rec treeFold func init treemap = match treemap with 
      | Leaf -> init
      | Branch(h, t) -> List.fold_left (treeFold func) (func init h) t;;

let queueMap func queue = match queue with 
      | Queue([], []) ->Queue([], [])
      | Queue(h, t) -> Queue((List.map func h),List.map func t);;

let queueFold func init queuemap = match queuemap with
      | Queue([], []) -> init
      | Queue(h, t) -> List.fold_left func (List.fold_left func init h) t ;;
