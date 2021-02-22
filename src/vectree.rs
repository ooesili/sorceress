// Sorceress
// Copyright (C) 2021  Wesley Merkel
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.

//! A multidimensional vector with dynamically determined depth.
//!
//! This modules provides the [`VecTree`] data structure which can store arbitrarily nested
//! vectors, each with dynamically determined depth. This means that each element of a `VecTree` may
//! be either a single element or another VecTree.

use std::convert::From;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum VecTree<T> {
    Leaf(T),
    Branch(Vec<VecTree<T>>),
}

impl<T> From<T> for VecTree<T> {
    fn from(x: T) -> Self {
        Self::Leaf(x)
    }
}

impl<T> From<Vec<VecTree<T>>> for VecTree<T> {
    fn from(trees: Vec<VecTree<T>>) -> Self {
        Self::Branch(trees)
    }
}

impl<T> VecTree<T> {
    pub fn get_path(&self, path: &[usize]) -> Option<&T> {
        match self {
            Self::Leaf(value) => Some(value),
            Self::Branch(children) => {
                if children.is_empty() {
                    return None;
                }
                path.get(0)
                    .and_then(|index| children[index % children.len()].get_path(&path[1..]))
            }
        }
    }

    pub fn space(trees: &[VecTree<T>]) -> Vec<usize> {
        let tree_dimensions = trees.iter().map(|tree| tree.size()).collect::<Vec<_>>();
        let mut result = Self::merge_spaces(&tree_dimensions);
        result.reverse();
        result
    }

    fn size(&self) -> Vec<usize> {
        match self {
            Self::Leaf(_) => vec![],
            Self::Branch(trees) => {
                let tree_dimensions = trees.iter().map(|tree| tree.size()).collect::<Vec<_>>();
                let mut result = Self::merge_spaces(&tree_dimensions);
                result.push(trees.len());
                result
            }
        }
    }

    fn merge_spaces(spaces: &[Vec<usize>]) -> Vec<usize> {
        let max_depth = spaces.iter().map(|space| space.len()).max().unwrap_or(0);
        (0..max_depth)
            .map(|depth| {
                spaces
                    .iter()
                    .map(|space| *space.get(depth).unwrap_or(&0))
                    .max()
                    .unwrap_or(0)
            })
            .collect()
    }

    pub fn map<F, B>(self, mut f: F) -> VecTree<B>
    where
        F: FnMut(T) -> B,
    {
        self.map_inner(&mut f)
    }

    fn map_inner<F, B>(self, f: &mut F) -> VecTree<B>
    where
        F: FnMut(T) -> B,
    {
        match self {
            VecTree::Leaf(x) => VecTree::Leaf(f(x)),
            VecTree::Branch(trees) => {
                VecTree::Branch(trees.into_iter().map(|tree| tree.map_inner(f)).collect())
            }
        }
    }

    pub fn flat_map<F, B>(self, mut f: F) -> VecTree<B>
    where
        F: FnMut(T) -> VecTree<B>,
    {
        self.flat_map_inner(&mut f)
    }

    fn flat_map_inner<F, B>(self, f: &mut F) -> VecTree<B>
    where
        F: FnMut(T) -> VecTree<B>,
    {
        match self {
            VecTree::Leaf(x) => f(x),
            VecTree::Branch(trees) => VecTree::Branch(
                trees
                    .into_iter()
                    .map(|tree| tree.flat_map_inner(f))
                    .collect(),
            ),
        }
    }
}

impl<T> IntoIterator for VecTree<T> {
    type Item = T;
    type IntoIter = Iter<T>;

    fn into_iter(self) -> Self::IntoIter {
        Iter {
            stack: vec![vec![self]],
        }
    }
}

pub struct Iter<T> {
    stack: Vec<Vec<VecTree<T>>>,
}

impl<T> Iter<T> {
    fn next_tree(&mut self) -> Option<VecTree<T>> {
        loop {
            if self.stack.is_empty() {
                return None;
            }
            let last_index = self.stack.len() - 1;
            match self.stack[last_index].pop() {
                Some(tree) => return Some(tree),
                None => self.stack.pop(),
            };
        }
    }
}

impl<T> Iterator for Iter<T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.next_tree()? {
                VecTree::Leaf(x) => return Some(x),
                VecTree::Branch(mut trees) => {
                    trees.reverse();
                    self.stack.push(trees)
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn space_test() {
        let empty_dimensions: Vec<usize> = vec![];
        let empty_tree_list: Vec<VecTree<()>> = vec![];
        assert_eq!(empty_dimensions, VecTree::space(&empty_tree_list));
        assert_eq!(empty_dimensions, VecTree::space(&[leaf(())]));
        assert_eq!(empty_dimensions, VecTree::space(&[leaf(()), leaf(())]));
        assert_eq!(vec![0], VecTree::space(&[branch::<()>(&[])]));
        assert_eq!(vec![1], VecTree::space(&[branch(&[leaf(())])]));
        assert_eq!(vec![2], VecTree::space(&[branch(&[leaf(()), leaf(())])]));
        assert_eq!(
            vec![1, 1],
            VecTree::space(&[branch(&[branch(&[leaf(())])])])
        );
        assert_eq!(
            vec![1, 2],
            VecTree::space(&[branch(&[branch(&[leaf(()), leaf(())])])])
        );
        assert_eq!(
            vec![2, 2],
            VecTree::space(&[branch(&[
                branch(&[leaf(()), leaf(())]),
                branch(&[leaf(()), leaf(())])
            ])])
        );
        assert_eq!(
            vec![1, 2, 3],
            VecTree::space(&[branch(&[branch(&[
                branch(&[leaf(()), leaf(()), leaf(())]),
                leaf(())
            ])])])
        );
        assert_eq!(
            vec![2, 2, 3],
            VecTree::space(&[branch(&[
                branch(&[leaf(()), branch(&[leaf(()), leaf(()), leaf(())])]),
                branch(&[branch(&[leaf(()), leaf(()), leaf(())]), leaf(())])
            ])])
        );
    }

    #[test]
    fn get_path_test() {
        // leafs always return the value
        assert_eq!(Some(&true), leaf(true).get_path(&[]));
        assert_eq!(Some(&true), leaf(true).get_path(&[0]));
        assert_eq!(Some(&true), leaf(true).get_path(&[1]));
        assert_eq!(Some(&true), leaf(true).get_path(&[0, 0]));
        assert_eq!(Some(&true), leaf(true).get_path(&[1, 1]));

        // branch indexing
        assert_eq!(Some(&true), branch(&[leaf(true)]).get_path(&[0]));
        assert_eq!(
            Some(&true),
            branch(&[leaf(false), leaf(false), leaf(true)]).get_path(&[5])
        );

        // branch and one past a leaf together
        assert_eq!(Some(&true), branch(&[leaf(true)]).get_path(&[0, 0]));

        // complex example
        assert_eq!(
            Some(&true),
            branch(&[
                leaf(false),
                leaf(false),
                branch(&[leaf(false), branch(&[leaf(true)])])
            ])
            .get_path(&[2, 1, 0])
        );

        // examples that return none
        let empty_branch: VecTree<()> = branch(&[]);
        assert_eq!(None, empty_branch.get_path(&[0]));
        assert_eq!(None, branch(&[leaf(())]).get_path(&[]))
    }

    #[test]
    fn iterator_test() {
        let empty: Vec<i32> = vec![];
        let empty_branch: VecTree<i32> = branch(&[]);
        assert_eq!(empty, to_vec(empty_branch));
        assert_eq!(vec![1], to_vec(leaf(1)));
        assert_eq!(vec![1], to_vec(branch(&[leaf(1)])));
        assert_eq!(vec![1, 2], to_vec(branch(&[leaf(1), leaf(2)])));
        assert_eq!(vec![1, 2], to_vec(branch(&[branch(&[leaf(1)]), leaf(2)])));
    }

    fn to_vec<T>(tree: VecTree<T>) -> Vec<T> {
        tree.into_iter().collect()
    }

    fn branch<T>(children: &[VecTree<T>]) -> VecTree<T>
    where
        T: Clone,
    {
        VecTree::Branch(children.into())
    }

    fn leaf<T>(value: T) -> VecTree<T> {
        VecTree::Leaf(value)
    }
}
