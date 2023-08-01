extern crate pathfinding;

use std::collections::{HashSet, HashMap};

use pathfinding::prelude::{dijkstra};

#[derive(Clone, Debug)]
struct Graph {
    edges: HashMap<usize, Cost1>,
}

#[derive(Clone, Debug)]
struct Cost1 {
    _nodes: HashSet<usize>,
}

impl Cost1 {
    pub fn new() -> Self {
        Cost1 {
            _nodes: HashSet::new(),
        }
    }

    pub fn insert(&mut self, node: usize) -> bool {
        self._nodes.insert(node)
    }
}

impl IntoIterator for Cost1 {
    type Item = (usize, u8);

    type IntoIter = std::vec::IntoIter<(usize, u8)>;

    fn into_iter(self) -> Self::IntoIter {
        self._nodes.iter().map(|n| (*n, 1)).collect::<Vec<_>>().into_iter()
    }
}

impl Graph {
    pub fn new() -> Self {
        Graph {
            edges: HashMap::new(),
        }
    }

    pub fn add_edge(&mut self, from: usize, to: usize) {
        self.edges.entry(from).or_insert(Cost1::new()).insert(to);
    }

    pub fn direct_neighbors(&self, node: &usize) -> Cost1 {
        self.edges.get(node).cloned().unwrap_or_else(Cost1::new)
    }

    pub fn dijkstra(&self) -> u8 {
        const START: usize = 0;
        let end = *self.edges.keys().max().unwrap();
        let mut dist = vec![usize::max_value(); end + 1];
        dist[START] = 0;
        match dijkstra(&START, |n| self.direct_neighbors(n), |e| *e == end) {
            Some((_path, cost)) => cost,
            None => u8::max_value(),
        }
    }
}




const JUMP: [u8; 24] = [2, 3, 1, 6, 4, 2, 2, 3, 1, 0, 2, 2, 0, 1, 4, 2, 2, 2, 3, 4, 1, 2, 6, 4];

fn main() {
    let mut g = Graph::new();
    for i in 0..JUMP.len() {
        'inner: for j in 1..=JUMP[i] {
            let d = i + j as usize;
            if d < JUMP.len() {
                g.add_edge(i, d);
            } else {
                break 'inner;
            }
        }
    }
    let dist = g.dijkstra();
    println!("{}", dist);
}
