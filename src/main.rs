#![feature(array_windows)]

use serde::Deserialize;
use std::{
    borrow::Cow,
    collections::{hash_map::Entry, BinaryHeap, HashMap}, fmt::Display,
};
use toml::from_str;

#[derive(Deserialize, Clone, Debug)]
pub struct Transaction {
    pub from: String,
    pub to: String,
    pub value: f64,
}

#[derive(Deserialize, Clone, Debug)]
pub struct InternationalCost {
    pub to: String,
    pub conversion_loss: f64,
    pub fee: f64,
}

#[derive(Deserialize)]
pub struct FinanceInformationDeser {
    international_costs: HashMap<String, Vec<InternationalCost>>,
    required_transactions: Vec<Transaction>,
    people_locations: HashMap<String, String>,
}

pub struct Finance<'a> {
    pub international_costs: HashMap<&'a str, Cow<'a, Vec<InternationalCost>>>,
    pub required_transactions: Cow<'a, Vec<Transaction>>,
    pub people_locations: Cow<'a, HashMap<String, String>>,
}

impl<'a> Finance<'a> {
    pub fn from_deser(value: &'a FinanceInformationDeser) -> Self {
        Self {
            international_costs: value
                .international_costs
                .iter()
                .map(|(text, value)| (text.as_str(), Cow::Borrowed(value)))
                .collect(),
            required_transactions: Cow::Borrowed(&value.required_transactions),
            people_locations: Cow::Borrowed(&value.people_locations),
        }
    }
}

#[derive(Debug)]
pub enum SimplificationError<'a> {
    MissingLocation { person: &'a str },
    NoSolution { from: &'a str, to: &'a str },
}

#[derive(PartialEq, Debug)]
struct QueueItem<'a> {
    node: &'a str,
    path: Vec<&'a str>,
    current_value: f64,
    losses: f64,
    true_value: f64,
}

impl<'a> Ord for QueueItem<'a> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        other.losses.partial_cmp(&self.losses).unwrap()
    }
}

impl<'a> PartialOrd for QueueItem<'a> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<'a> Eq for QueueItem<'a> {}

#[derive(Debug)]
pub struct SimplifiedTransactions {
    pub transactions: Vec<Transaction>,
    pub total_losses: f64
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


impl<'a> Finance<'a> {
    pub fn simplify_transactions(&self) -> Result<SimplifiedTransactions, SimplificationError> {
        let mut people_transactions: HashMap<(&str, &str), f64> = HashMap::new();

        for tx in self.required_transactions.iter() {
            let key = (tx.from.as_str(), tx.to.as_str());
            if let Some(mutref) = people_transactions.get_mut(&key) {
                *mutref += tx.value;
            } else if let Some(mutref) =
                people_transactions.get_mut(&(tx.to.as_str(), tx.from.as_str()))
            {
                *mutref -= tx.value
            } else {
                people_transactions.insert(key, tx.value);
            }
        }

        people_transactions = people_transactions
            .into_iter()
            .map(|((from, to), value)| {
                if value < 0.0 {
                    ((to, from), -value)
                } else {
                    ((from, to), value)
                }
            })
            .collect();

        println!("{people_transactions:?}");
        let mut country_transactions: HashMap<(&str, &str), f64> = HashMap::new();

        for ((from, to), value) in &people_transactions {
            let Some(from) = self.people_locations.get(*from) else {
                return Err(SimplificationError::MissingLocation { person: from })
            };
            let Some(to) = self.people_locations.get(*to) else {
                return Err(SimplificationError::MissingLocation { person: to })
            };
            if from == to {
                // todo
                continue;
            }
            let key = (from.as_str(), to.as_str());
            if let Some(mutref) = country_transactions.get_mut(&key) {
                *mutref += value;
            } else if let Some(mutref) = country_transactions.get_mut(&(to.as_str(), from.as_str()))
            {
                *mutref -= value
            } else {
                country_transactions.insert(key, *value);
            }
        }

        country_transactions = country_transactions
            .into_iter()
            .map(|((from, to), value)| {
                if value < 0.0 {
                    ((to, from), -value)
                } else {
                    ((from, to), value)
                }
            })
            .collect();

        println!("{country_transactions:?}");

        let mut country_transaction_paths = vec![];

        for ((from, to), value) in country_transactions {
            let mut priority_queue = BinaryHeap::new();
            priority_queue.push(QueueItem {
                node: from,
                path: vec![],
                current_value: value,
                losses: 0.0,
                true_value: value,
            });

            let mut final_item = None;

            while let Some(item) = priority_queue.pop() {
                if item.node == to {
                    final_item = Some(item);
                    break;
                }

                let Some(successors) = self.international_costs.get(item.node) else {
                    continue
                };
                let mut path = item.path.clone();
                path.push(item.node);

                for successor_cost in successors.iter() {
                    let conversion_loss = item.current_value * successor_cost.conversion_loss;

                    let current_value = item.current_value + conversion_loss;
                    let losses = item.losses + conversion_loss + successor_cost.fee;

                    priority_queue.push(QueueItem {
                        node: &successor_cost.to,
                        path: path.clone(),
                        current_value,
                        losses,
                        true_value: value,
                    })
                }
            }

            let Some(final_item) = final_item else {
                return Err(SimplificationError::NoSolution { from, to })
            };

            country_transaction_paths.push(final_item);
        }

        let mut total_losses = 0.0;
        country_transactions = HashMap::new();
        for item in country_transaction_paths {
            let mut path = item.path;
            path.push(item.node);
            total_losses += item.losses;

            for [from, to] in path.array_windows::<2>() {
                let key = (*from, *to);
                if let Some(mutref) = country_transactions.get_mut(&key) {
                    *mutref += item.true_value;
                } else if let Some(mutref) = country_transactions.get_mut(&(*to, *from)) {
                    *mutref -= item.true_value;
                } else {
                    country_transactions.insert(key, item.true_value);
                }
            }
        }

        country_transactions = country_transactions
            .into_iter()
            .map(|((from, to), value)| {
                if value < 0.0 {
                    ((to, from), -value)
                } else {
                    ((from, to), value)
                }
            })
            .collect();

        println!("{country_transactions:?}");

        let mut country_people_map: HashMap<&str, Vec<&str>> = HashMap::new();
        for (person, country) in self.people_locations.iter() {
            match country_people_map.entry(country.as_str()) {
                Entry::Occupied(mut entry) => entry.get_mut().push(person.as_str()),
                Entry::Vacant(entry) => {
                    entry.insert(vec![person.as_str()]);
                }
            }
        }

        let transactions = country_transactions
            .into_iter()
            .map(|((from, to), value)| {
                let from = country_people_map
                    .get(from)
                    .unwrap()
                    .get(0)
                    .unwrap()
                    .to_string();
                let to = country_people_map
                    .get(to)
                    .unwrap()
                    .get(0)
                    .unwrap()
                    .to_string();
                Transaction { from, to, value }
            })
            .chain(
                country_people_map
                    .iter()
                    .map(
                        |(_, people)| {
                            people
                                .iter()
                                .skip(1)
                                .map(|person| {
                                    let value: f64 = people_transactions
                                        .iter()
                                        .map(|((from, to), value)| {
                                            if from == person {
                                                *value
                                            } else if to == person {
                                                - value
                                            } else {
                                                0.0
                                            }
                                        })
                                        .sum();

                                    if value < 0.0 {
                                        Transaction { from: people.get(0).unwrap().to_string(), to: person.to_string(), value: - value }
                                    } else {
                                        Transaction { to: people.get(0).unwrap().to_string(), from: person.to_string(), value }
                                    }
                                })
                            
                        }
                    )
                    .flatten()
            )
            .collect::<Vec<_>>();

        Ok(SimplifiedTransactions { transactions, total_losses })
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

    let finance: FinanceInformationDeser = match from_str(&text) {
        Ok(x) => x,
        Err(e) => {
            println!("Faced the following toml error: {e:?}");
            return;
        }
    };

    let finance = Finance::from_deser(&finance);

    match finance.simplify_transactions() {
        Ok(ans) => {
            println!("{ans}");
        }
        Err(e) => {
            println!("Faced the following error: {e:?}");
        }
    }
}
