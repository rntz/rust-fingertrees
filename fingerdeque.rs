use fingertree::*;
mod fingertree;

struct Elem<A>(A);
struct Deque<A> {
    tree: ~Tree<Size,Elem<A>>
}

impl<A> Measured<Size> for Elem<A> {
    fn measure(&self) -> Size { return Size(1) }
}

impl<A> Deque<A> {
    pub fn len(&self) -> uint { *self.tree.measure() }
    pub fn is_empty(&self) -> bool { self.tree.is_empty() }

    pub fn head_opt<'a>(&'a mut self) -> Option<&'a mut A> {
        match self.tree.head_opt() {
            None => None,
            Some(&Elem(ref mut x)) => Some(x)
        }
    }
    pub fn last_opt<'a>(&'a mut self) -> Option<&'a mut A> {
        match self.tree.last_opt() {
            None => None,
            Some(&Elem(ref mut x)) => Some(x)
        }
    }

    pub fn head<'a>(&'a mut self) -> &'a mut A { self.head_opt().unwrap() }
    pub fn last<'a>(&'a mut self) -> &'a mut A { self.last_opt().unwrap() }

    pub fn push(&mut self, x: A) {
        let tree = std::util::replace(&mut self.tree, ~Tree::empty());
        self.tree = tree.cons_right(~Leaf(Elem(x)));
    }
    pub fn unshift(&mut self, x: A) {
        let tree = std::util::replace(&mut self.tree, ~Tree::empty());
        self.tree = tree.cons_left(~Leaf(Elem(x)));
    }

    pub fn pop_opt(&mut self) -> Option<A> {
        let tree = std::util::replace(&mut self.tree, ~Tree::empty());
        match tree.view_left() {
            None => None,
            Some((~Leaf(a), t)) => { self.tree = t; Some(*a) }
            Some((_, _)) => unreachable!()
        }
    }
}

fn main() {
}
