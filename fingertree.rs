pub trait Monoid {
    fn unit() -> Self;
    fn join(&self, x: &Self) -> Self;
}

// Useful operations on ~[].
fn append_move<T>(mut x: ~[T], y: ~[T]) -> ~[T] { x.push_all_move(y); x }

// A useful monoid.
pub struct Size(u64);
impl Monoid for Size {
    fn unit() -> Size { return Size(0); }
    fn join(&self, x: &Size) -> Size {
        return Size(**self + **x);
    }
}

// Another useful monoid
pub trait Measured<V> {
    fn measure(&self) -> V;
}


// ===== 2-3 tree nodes =====
enum Node<V,A> {
    Leaf(A),
    // invariant: exactly 2 or 3 elements
    Node2(V, ~Node<V,A>, ~Node<V,A>),
    Node3(V, ~Node<V,A>, ~Node<V,A>, ~Node<V,A>),
}

impl<V:Monoid + Clone, A:Measured<V>> Measured<V> for Node<V,A> {
    fn measure(&self) -> V {
        match *self {
            Leaf(ref x) => x.measure(),
            Node2(ref v, _, _) => v.clone(),
            Node3(ref v, _, _, _) => v.clone(),
        }
    }
}

impl<V:Monoid+Clone, A:Measured<V>> Node<V,A> {
    fn new2(x: ~Node<V,A>, y: ~Node<V,A>) -> ~Node<V,A> {
        ~Node2(x.measure().join(&y.measure()), x, y)
    }

    fn new3(x: ~Node<V,A>, y: ~Node<V,A>, z: ~Node<V,A>) -> ~Node<V,A> {
        ~Node3(x.measure().join(&y.measure()).join(&z.measure()), x, y, z)
    }
}

// ===== Digits =====
// invariant: between 1 and 4 elements, inclusive
struct Digit<V,A>(~[~Node<V,A>]);

impl<V:Monoid+Clone, A:Measured<V>> Measured<V> for Digit<V,A> {
    fn measure(&self) -> V {
        let mut iter = self.iter();
        let mut v = match iter.next() { Some(x) => x.measure(),
                                        None => unreachable!() };
        for x in iter {
            v = v.join(&x.measure());
        }
        return v;
    }
}

// ===== Trees =====
enum Tree<V,A> {
    Empty,
    Single(~Node<V,A>),
    Deep(V, Digit<V,A>, ~Tree<V,A>, Digit<V,A>),
}

impl<V:Monoid + Clone, A: Measured<V>> Measured<V> for Tree<V,A> {
    fn measure(&self) -> V {
        match *self {
            Empty => Monoid::unit(),
            Single(ref n) => n.measure(),
            Deep(ref v, _, _, _) => v.clone(),
        }
    }
}

impl<V: Monoid + Clone, A:Measured<V>> Tree<V,A> {
    pub fn empty() -> Tree<V,A> { Empty }
    pub fn singleton(x: A) -> Tree<V,A> { Single(~Leaf(x)) }
    pub fn is_empty(&self) -> bool { match *self { Empty => true, _ => false } }

    // Hopefully this'll get inlined.
    fn deep(pre: Digit<V,A>, mid: ~Tree<V,A>, suf: Digit<V,A>) -> ~Tree<V,A> {
        let v = pre.measure().join(&mid.measure()).join(&suf.measure());
        ~Deep(v, pre, mid, suf)
    }

    fn cons_left(~self, x: ~Node<V,A>) -> ~Tree<V,A> {
        match *self {
            Empty => { ~Single(x) }
            Single(b) => { Tree::deep(Digit(~[x]), ~Empty, Digit(~[b])) }
            Deep(v, pre, mid, suf) => {
                let v = x.measure().join(&v);
                let (pre, mid) = match *pre {
                    [b,c,d,e] => (~[x,b], mid.cons_left(Node::new3(c,d,e))),
                    ps => (append_move(~[x], ps), mid)
                };
                ~Deep(v, Digit(pre), mid, suf)
            }
        }
    }

    fn cons_right(~self, x: ~Node<V,A>) -> ~Tree<V,A> {
        match *self {
            Empty => { ~Single(x) }
            Single(a) => { Tree::deep(Digit(~[a]), ~Empty, Digit(~[x])) }
            Deep(v, pre, mid, suf) => {
                let v = x.measure().join(&v);
                let (mid, suf) = match *suf {
                    [a,b,c,d] => (mid.cons_right(Node::new3(a,b,c)), ~[d,x]),
                    ss => (mid, append_move(ss, ~[x]))
                };
                ~Deep(v, pre, mid, Digit(suf))
            }
        }
    }
}

fn main() {
}
