#![feature(array_windows)]

use fxhash::{FxHashMap as HashMap, FxHashSet as HashSet};
use serde::Deserialize;
use std::{
    cmp::Ordering,
    collections::{hash_map::Entry, BinaryHeap},
    fmt::Display,
    mem::take,
    rc::Rc,
};
use toml::from_str;

#[derive(Deserialize, Clone, Debug)]
pub struct Transaction {
    pub from: String,
    pub to: String,
    pub value: f64,
}

#[derive(Deserialize, Clone, Copy, Debug)]
pub struct InternationalCost {
    pub conversion_loss: f64,
    pub fee: f64,
}

#[derive(Deserialize)]
pub struct Finance {
    pub international_costs: HashMap<String, HashMap<String, InternationalCost>>,
    pub required_transactions: HashMap<String, HashMap<String, Vec<f64>>>,
    pub people_locations: HashMap<String, String>,
}

#[derive(Debug)]
pub enum SimplificationError<'a> {
    MissingLocation { person: &'a str },
    NoSolution { from: &'a str, to: &'a str },
}

#[derive(PartialEq, Debug)]
struct NodeConnection<'a> {
    from: Rc<Node<'a>>,
    international_cost: Option<f64>,
}

#[derive(PartialEq, Debug)]
struct Node<'a> {
    item: &'a str,
    chain_length: usize,
    from: Option<NodeConnection<'a>>,
    total_losses: f64,
}

impl<'a> Node<'a> {
    fn new(item: &'a str) -> Rc<Self> {
        Rc::new(Self {
            item,
            chain_length: 0,
            from: None,
            total_losses: 0.0,
        })
    }

    fn extend_international<'b>(
        self: &Rc<Self>,
        item: &'a str,
        value: f64,
        international_cost: InternationalCost,
    ) -> Rc<Self> {
        let international_cost = (value + self.total_losses) * international_cost.conversion_loss
            + international_cost.fee;
        Rc::new(Self {
            item,
            chain_length: self.chain_length + 1,
            from: Some(NodeConnection {
                from: self.clone(),
                international_cost: Some(international_cost),
            }),
            total_losses: self.total_losses + international_cost,
        })
    }

    fn extend_international_no_fee<'b>(
        self: &Rc<Self>,
        item: &'a str,
        value: f64,
        international_cost: InternationalCost,
    ) -> Rc<Self> {
        let international_cost = (value + self.total_losses) * international_cost.conversion_loss;
        Rc::new(Self {
            item,
            chain_length: self.chain_length + 1,
            from: Some(NodeConnection {
                from: self.clone(),
                international_cost: Some(international_cost),
            }),
            total_losses: self.total_losses + international_cost,
        })
    }

    fn extend<'b>(self: &Rc<Self>, item: &'a str) -> Rc<Self> {
        Rc::new(Self {
            item,
            chain_length: self.chain_length + 1,
            from: Some(NodeConnection {
                from: self.clone(),
                international_cost: None,
            }),
            total_losses: self.total_losses,
        })
    }

    fn chain_contains(self: &Rc<Self>, item: &str) -> bool {
        if self.item == item {
            return true;
        }
        self.from
            .as_ref()
            .map(|conn| conn.from.chain_contains(item))
            .unwrap_or_default()
    }
}

impl<'a> Ord for Node<'a> {
    fn cmp(&self, other: &Self) -> Ordering {
        match other.total_losses.partial_cmp(&self.total_losses).unwrap() {
            Ordering::Equal => other.chain_length.cmp(&self.chain_length),
            ord => ord,
        }
    }
}

impl<'a> PartialOrd for Node<'a> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<'a> Eq for Node<'a> {}

#[derive(Debug)]
pub struct SimplifiedTransactions {
    pub transactions: Vec<Transaction>,
    pub total_losses: f64,
}

impl Display for SimplifiedTransactions {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Simplified Transactions")?;
        for tx in &self.transactions {
            writeln!(f, "{} to {}: {}", tx.from, tx.to, tx.value)?;
        }
        writeln!(f, "Total losses: {}", self.total_losses)
    }
}

impl Finance {
    pub fn simplify_transactions(&self) -> Result<SimplifiedTransactions, SimplificationError> {
        let mut simplified_required_transactions: HashMap<_, f64> = self
            .required_transactions
            .iter()
            .map(|(from, to_map)| {
                to_map
                        .iter()
                        .map(|(to, values)|
                        ((from.as_str(), to.as_str()), values.iter().sum()))
            })
            .flatten()
            .collect();

        take(&mut simplified_required_transactions)
            .into_iter()
            .for_each(|((from, to), value)| 
                match simplified_required_transactions.entry((from, to)) {
                    Entry::Occupied(mut current_value) => *current_value.get_mut() += value,

                    Entry::Vacant(_) => match simplified_required_transactions.entry((to, from)) {
                        Entry::Occupied(mut current_value) => *current_value.get_mut() -= value,

                        Entry::Vacant(entry) => {
                            entry.insert(- value);
                        }
                    }
                }
            );

        let mut total_losses = 0.0;
        let mut existing_international_segments: HashSet<(&str, &str)> = HashSet::default();
        let mut country_people_map: HashMap<&str, Vec<&str>> = HashMap::default();

        self.people_locations.iter().for_each(|(person, country)| {
            match country_people_map.entry(country) {
                Entry::Occupied(mut entry) => entry.get_mut().push(person),
                Entry::Vacant(entry) => {
                    entry.insert(vec![person]);
                }
            }
        });

        'outer: for ((mut from, mut to), mut value) in take(&mut simplified_required_transactions) {
            if value < 0.0 {
                let tmp = from;
                from = to;
                to = tmp;
                value *= -1.0;
            }

            let mut priority_queue = BinaryHeap::new();
            priority_queue.push(Node::new(from));

            while let Some(node) = priority_queue.pop() {
                if node.item == to {
                    let mut running_cost = 0.0;
                    let mut node = node;
                    loop {
                        let Some(connection) = node.from.as_ref() else {
                            break
                        };
                        let from = connection.from.item;
                        let to = node.item;
                        let value = value + running_cost;

                        match simplified_required_transactions.entry((from, to)) {
                            Entry::Occupied(mut current_value) => *current_value.get_mut() += value,
        
                            Entry::Vacant(_) => match simplified_required_transactions.entry((to, from)) {
                                Entry::Occupied(mut current_value) => *current_value.get_mut() -= value,
        
                                Entry::Vacant(entry) => {
                                    entry.insert(- value);
                                }
                            }
                        }
                        if let Some(cost) = connection.international_cost.clone() {
                            existing_international_segments.insert((from, to));
                            total_losses += cost;
                            running_cost += cost;
                        }
                        node = connection.from.clone();
                    }
                    continue 'outer;
                }

                let Some(location) = self.people_locations.get(node.item) else {
                    return Err(SimplificationError::MissingLocation { person: node.item })
                };

                country_people_map
                    .get(location.as_str())
                    .unwrap()
                    .iter()
                    .for_each(|other| {
                        if !node.chain_contains(other) {
                            priority_queue.push(node.extend(other));
                        }
                    });

                let Some(successors) = self.international_costs.get(location) else {
                    continue
                };

                successors
                    .iter()
                    .for_each(|(successor, international_cost)| {
                        country_people_map
                            .get(successor.as_str())
                            .unwrap()
                            .iter()
                            .for_each(|other| 
                                if existing_international_segments.contains(&(node.item, *other)) || existing_international_segments.contains(&(*other, node.item)){
                                    priority_queue.push(node.extend_international_no_fee(
                                        other,
                                        value,
                                        *international_cost,
                                    ));
                                } else if !node.chain_contains(other) {
                                    priority_queue.push(node.extend_international(
                                        other,
                                        value,
                                        *international_cost,
                                    ));
                                }
                            )
                    });
            }

            return Err(SimplificationError::NoSolution { from, to });
        }

        Ok(SimplifiedTransactions {
            transactions: simplified_required_transactions
                .into_iter()
                .map(|((from, to), value)| {
                    if value < 0.0 {
                        Transaction {
                            to: from.into(),
                            from: to.into(),
                            value: -value,
                        }
                    } else {
                        Transaction {
                            from: from.into(),
                            to: to.into(),
                            value,
                        }
                    }
                })
                .collect(),
            total_losses,
        })
    }
}

fn main() {
    let text = match std::fs::read_to_string("finance.toml") {
        Ok(x) => x,
        Err(e) => {
            eprintln!("Faced the following error while reading finance.toml: {e:?}");
            return;
        }
    };

    let finance: Finance = match from_str(&text) {
        Ok(x) => x,
        Err(e) => {
            println!("Faced the following toml error: {e:?}");
            return;
        }
    };

    match finance.simplify_transactions() {
        Ok(ans) => {
            println!("{ans}");
        }
        Err(e) => {
            println!("Faced the following error: {e:?}");
        }
    }
}
