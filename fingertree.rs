// ========== Traits and Utilities ==========
pub trait Monoid {
    fn unit() -> Self;
    fn join(&self, x: &Self) -> Self;
}

pub trait Measured<V> {
    fn measure(&self) -> V;
}

// A useful monoid.
pub struct Size(u64);
impl Monoid for Size {
    fn unit() -> Size { return Size(0); }
    fn join(&self, x: &Size) -> Size {
        return Size(**self + **x);
    }
}

// Useful operations on ~[].
fn append_move<T>(mut x: ~[T], y: ~[T]) -> ~[T] { x.push_all_move(y); x }
fn split_at<T>(x: ~[T], mut i: uint) -> (~[T], T, ~[T]) {
    // TODO?: more efficient implementation
    assert!(i < x.len());
    let mut a = ~[];
    a.reserve(i);
    let mut iter = x.move_iter();
    while i > 0 {
        a.push(iter.next().unwrap());
        i -= 1;
    }
    let v = iter.next().unwrap();
    return (a, v, iter.collect());
}

impl<V:Monoid, A:Measured<V>> Measured<V> for ~[A] {
    fn measure(&self) -> V {
        let mut v: V = Monoid::unit();
        for x in self.iter() {
            v = v.join(&x.measure());
        }
        return v;
    }
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

impl<V:Monoid + Clone, A:Measured<V>> Measured<V> for ~Node<V,A> {
    fn measure(&self) -> V { (*self).measure() }
}

impl<V:Monoid+Clone, A:Measured<V>> Node<V,A> {
    fn new2(x: ~Node<V,A>, y: ~Node<V,A>) -> ~Node<V,A> {
        ~Node2(x.measure().join(&y.measure()), x, y)
    }

    fn new3(x: ~Node<V,A>, y: ~Node<V,A>, z: ~Node<V,A>) -> ~Node<V,A> {
        ~Node3(x.measure().join(&y.measure()).join(&z.measure()), x, y, z)
    }

    fn head<'a>(&'a mut self) -> &'a mut A {
        match *self {
            Leaf(ref mut a) => a,
            Node2(_, ref mut x, _) => x.head(),
            Node3(_, ref mut x, _, _) => x.head(),
        }
    }

    fn to_digit(self) -> Digit<V,A> {
        match self {
            Node2(_,x,y) => Digit(~[x,y]),
            Node3(_,x,y,z) => Digit(~[x,y,z]),
            Leaf(_) => unreachable!()
        }
    }

    fn lookup<'a>(&'a mut self, v: V, p: &fn(&V) -> bool)
        -> (V, &'a mut ~Node<V,A>)
    {
        match self {
            &Leaf(_) => unreachable!(),
            &Node2(_, ref mut x, ref mut y) => {
                let vx = v.join(&x.measure());
                if p(&vx) { (v, x) }
                else { (vx, y) }
            }
            &Node3(_, ref mut x, ref mut y, ref mut z) => {
                let vx = v.join(&x.measure());
                if p(&vx) { return (v, x) }
                let vy = vx.join(&y.measure());
                if p(&vy) { (vx, y) }
                else { (vy, z) }
            }
        }
    }

    fn split(~self, v: V, p: &fn(&V) -> bool)
             -> (~[~Node<V,A>], ~Node<V,A>, ~[~Node<V,A>])
    {
        match *self {
            Leaf(_) => unreachable!(),
            Node2(_, x, y) => {
                let vx = v.join(&x.measure());
                if p(&vx) { (~[], x, ~[y]) }
                else { (~[x], y, ~[]) }
            }
            Node3(_, x, y, z) => {
                let vx = v.join(&x.measure());
                if p(&vx) { return (~[], x, ~[y,z]) }
                let vy = vx.join(&y.measure());
                if p(&vy) { (~[x], y, ~[z]) }
                else { (~[x,y], z, ~[]) }
            }
        }
    }
}


// ===== Digits =====
// invariant: between 1 and 4 elements, inclusive
struct Digit<V,A>(~[~Node<V,A>]);

impl<V:Monoid+Clone, A:Measured<V>> Measured<V> for Digit<V,A> {
    fn measure(&self) -> V {
        return (*self).measure();
    }
}

impl<V:Monoid+Clone, A:Measured<V>> Digit<V,A> {
    fn head<'a>(&'a mut self) -> &'a mut A { (*self)[0].head() }

    fn to_tree(self) -> ~Tree<V,A> {
        match *self {
            [a] => ~Single(a),
            [a,b] => deep(Digit(~[a]), ~Empty, Digit(~[b])),
            [a,b,c] => deep(Digit(~[a,b]), ~Empty, Digit(~[c])),
            [a,b,c,d] => deep(Digit(~[a,b]), ~Empty, Digit(~[c,d])),
            _ => unreachable!()
        }
    }

    fn split_pos(&self, v: V, p: &fn(&V) -> bool) -> (V, Option<uint>) {
        let mut v = v;
        for (idx,elem) in (*self).iter().enumerate() {
            let vnew = v.join(&elem.measure());
            if p(&vnew) { return (v, Some(idx)) }
            v = vnew;
        }
        (v, None)
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

// Hopefully this'll get inlined.
fn deep<V: Monoid + Clone, A:Measured<V>>
   (pre: Digit<V,A>, mid: ~Tree<V,A>, suf: Digit<V,A>) -> ~Tree<V,A>
{
    let v = pre.measure().join(&mid.measure()).join(&suf.measure());
    ~Deep(v, pre, mid, suf)
}

impl<V: Monoid + Clone, A:Measured<V>> Tree<V,A> {
    pub fn empty() -> Tree<V,A> { Empty }
    pub fn singleton(x: A) -> Tree<V,A> { Single(~Leaf(x)) }
    pub fn is_empty(&self) -> bool { match *self { Empty => true, _ => false } }

    // ===== Consing =====
    fn cons_left(~self, x: ~Node<V,A>) -> ~Tree<V,A> {
        match *self {
            Empty => { ~Single(x) }
            Single(b) => { deep(Digit(~[x]), ~Empty, Digit(~[b])) }
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
            Single(a) => { deep(Digit(~[a]), ~Empty, Digit(~[x])) }
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

    // ===== Head & tail access =====
    fn head_opt<'a>(&'a mut self) -> Option<&'a mut A> {
        assert!(!self.is_empty());
        match *self {
            Empty => None,
            Single(ref mut a) => Some(a.head()),
            Deep(_, ref mut pre, _, _) => Some(pre.head()),
        }
    }

    fn last_opt<'a>(&'a mut self) -> Option<&'a mut A> {
        match *self {
            Empty => None,
            Single(ref mut a) => Some(a.head()),
            Deep(_, ref mut pre, _, _) => Some(pre.head()),
        }
    }

    fn head<'a>(&'a mut self) -> &'a mut A { self.head_opt().unwrap() }
    fn last<'a>(&'a mut self) -> &'a mut A { self.last_opt().unwrap() }

    // ===== Views/un =====
    fn viewL(~self) -> Option<(~Node<V,A>, ~Tree<V,A>)> {
        let mut x = self;
        // Have to do some gymnastics to satisfy the borrow-checker.
        let a = match *x {
            Empty => return None,
            Single(a) => return Some((a, ~Empty)),
            Deep(_, ref mut pre, _, _) => pre.shift(),
        };
        Some((a,x.deepL()))
    }

    fn deepL(~self) -> ~Tree<V,A> {
        let mut x = self;
        match x {
            ~Deep(_, Digit([]), mid, suf) => match mid.viewL() {
                None => return suf.to_tree(),
                Some((a, mid)) => {
                    // TODO?: make this in-place
                    return deep(a.to_digit(), mid, suf)
                }
            },
            ~Deep(ref mut v, ref pre, ref mid, ref suf) => {
                *v = pre.measure().join(&mid.measure()).join(&suf.measure());
            }
            _ => unreachable!()
        }
        x
    }

    fn viewR(~self) -> Option<(~Tree<V,A>, ~Node<V,A>)> {
        let mut x = self;
        // Have to do some gymnastics to satisfy the borrow-checker.
        let a = match *x {
            Empty => return None,
            Single(a) => return Some((~Empty, a)),
            Deep(_, _, _, ref mut suf) => suf.pop(),
        };
        Some((x.deepR(), a))
    }

    fn deepR(~self) -> ~Tree<V,A> {
        let mut x = self;
        match x {
            ~Deep(_, pre, mid, Digit([])) => match mid.viewR() {
                None => return pre.to_tree(),
                Some((mid, a)) => {
                    // TODO?: make this in-place
                    return deep(pre, mid, a.to_digit())
                }
            },
            ~Deep(ref mut v, ref pre, ref mid, ref suf) => {
                *v = pre.measure().join(&mid.measure()).join(&suf.measure());
            }
            _ => unreachable!()
        }
        x
    }

    fn append_array(~self, elems: ~[~Node<V,A>]) -> ~Tree<V,A> {
        let mut x = self;
        for elem in elems.move_iter() {
            x = x.cons_right(elem);
        }
        x
    }

    fn prepend_array(~self, elems: ~[~Node<V,A>]) -> ~Tree<V,A> {
        let mut x = self;
        for elem in elems.move_rev_iter() {
            x = x.cons_left(elem);
        }
        x
    }

    // ===== Concatenation =====
    fn append(~self, other: ~Tree<V,A>) -> ~Tree<V,A> { self.app3(~[], other) }

    fn app3(~self, elems: ~[~Node<V,A>], other: ~Tree<V,A>) -> ~Tree<V,A> {
        match (self, other) {
            (~Empty, snd) => snd.prepend_array(elems),
            (fst, ~Empty) => fst.append_array(elems),
            (~Single(a), snd) => snd.prepend_array(elems).cons_left(a),
            (fst, ~Single(b)) => fst.append_array(elems).cons_right(b),
            (~Deep(v1, pre1, mid1, Digit(suf1)),
             ~Deep(v2, Digit(pre2), mid2, suf2)) => {
                let v = v1.join(&elems.measure()).join(&v2);

                // Make a list of nodes from suf1, elems, and pre1
                let mut nodes = ~[];
                let mut left = suf1.len() + elems.len() + pre2.len();
                let mut iter = suf1.move_iter().chain(
                                   elems.move_iter()).chain(
                                   pre2.move_iter());
                assert!(left >= 2);
                while left > 0 {
                    let a = iter.next().unwrap();
                    let b = iter.next().unwrap();
                    match left {
                        2 => {
                            nodes.push(Node::new2(a,b));
                            break;
                        }
                        4 => {
                            nodes.push(Node::new2(a,b));
                            let c = iter.next().unwrap();
                            nodes.push(Node::new2(c, iter.next().unwrap()));
                            break;
                        }
                        1 => unreachable!(),
                        _ => {}
                    }
                    // Push the first three and continue
                    nodes.push(Node::new3(a, b, iter.next().unwrap()));
                    left -= 3;
                }

                let mid = mid1.app3(nodes, mid2);
                ~Deep(v, pre1, mid, suf2)
            }
        }
    }

    // ===== Splitting =====
    fn split(~self, p: &fn(&V) -> bool) -> (~Tree<V,A>, ~Tree<V,A>) {
        if self.is_empty() {
            return (~Empty, ~Empty);
        }
        if !p(&self.measure()) {
            return (self, ~Empty)
        }
        let (l,x,r) = self.splitNonempty(Monoid::unit(), p);
        (l, r.cons_left(x))
    }

    fn splitNonempty(~self, v: V, p: &fn(&V) -> bool)
        -> (~Tree<V,A>, ~Node<V,A>, ~Tree<V,A>)
    {
        assert!(!self.is_empty());
        let (pre, mid, suf) = match *self {
            Empty => unreachable!(),
            Single(a) => return (~Empty, a, ~Empty),
            Deep(_, pre, mid, suf) => (pre, mid, suf),
        };

        // Since measuring a digit just iterates over all its elements, we might
        // as well just try to split it.
        // We need to eta-expand p to avoid aggro from the borrow-checker (?!).
        let vpre = match pre.split_pos(v, |x| p(x)) {
            (v, None) => v,
            (_, Some(idx)) => {
                let (l,x,r) = split_at(*pre, idx);
                return ((~Empty).append_array(l),
                        x,
                        (deep(Digit(r),mid,suf)).deepL())
            }
        };
        assert!(!p(&vpre));

        let vmid = vpre.clone().join(&mid.measure());
        if p(&vmid) {
            let (ml, xs, mr) = mid.splitNonempty(vpre.clone(), |x| p(x));
            let (l, x, r) = xs.split(vpre.join(&ml.measure()), |x| p(x));
            return (deep(pre, ml, Digit(l)).deepR(),
                    x,
                    deep(Digit(r), mr, suf).deepL())
        }

        let (_,i) = suf.split_pos(vmid, p);
        // TODO: would it be safe to just use i.unwrap() here?
        let i = match i { None => (*suf).len() - 1, Some(i) => i };
        let (l,x,r) = split_at(*suf, i);
        (deep(pre, mid, Digit(l)).deepR(), x, (~Empty).append_array(r))
    }

    // ===== Lookup =====
    fn lookup<'a>(&'a mut self, p: &fn(&V) -> bool)
                  -> (V, Option<&'a mut ~Node<V,A>>)
    {
        if self.is_empty() { return (Monoid::unit(), None) }
        let v = self.measure();
        if !p(&v) { return (v, None) }
        let (v, x) = self.lookupUnsafe(Monoid::unit(), p);
        (v, Some(x))
    }

    fn lookupUnsafe<'a>(&'a mut self, v: V, p: &fn(&V) -> bool)
       -> (V, &'a mut ~Node<V,A>)
    {
        assert!(!self.is_empty());
        assert!(p(&v.join(&self.measure())));

        let (pre, mid, suf) = match *self {
            Empty => unreachable!(),
            Single(ref mut a) => return (v, a),
            Deep(_, ref mut pre, ref mut mid, ref mut suf) => (pre, mid, suf)
        };

        let vpre = match pre.split_pos(v, |x| p(x)) {
            (v, None) => v,
            (v, Some(idx)) => return (v, &mut (*pre)[idx])
        };
        assert!(!p(&vpre));

        let vmid = vpre.join(&mid.measure());
        if p(&vmid) {
            let (vl, xs) = mid.lookupUnsafe(vpre, |x| p(x));
            let (vx, i) = xs.lookup(vl, p);
            return (vx, i);
        }

        let (v,i) = suf.split_pos(vmid, p);
        (v, &mut (*suf)[i.unwrap()])
    }
}

fn main() {
}
