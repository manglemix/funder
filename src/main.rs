use std::collections::{HashMap, HashSet, hash_map::Entry};
use serde::Deserialize;


#[derive(Deserialize, Clone, Copy)]
struct Transaction<'a> {
    from: &'a str,
    to: &'a str,
    value: f64
}


#[derive(Deserialize)]
struct RawFinanceInformation<'a> {
    international_transfer_fee: HashMap<&'a str, f64>,
    currency_conversion_loss: HashMap<&'a str, f64>,
    required_transactions: Vec<Transaction<'a>>
}


struct Finance<'a> {
    international_transfer_fee: HashMap<(&'a str, &'a str), f64>,
    currency_conversion_loss: HashMap<(&'a str, &'a str), f64>,
    required_transactions: Vec<Transaction<'a>>
}


impl<'a> From<RawFinanceInformation<'a>> for Finance<'a> {
    fn from(value: RawFinanceInformation<'a>) -> Self {
        Self {
            international_transfer_fee:
                value
                    .international_transfer_fee
                    .into_iter()
                    .map(|(text, value| {
                        (text.split_at(mid), value)
                    })
        }
    }
}


fn main() {
    let finance: Finance = todo!();

    let mut countries = finance
        .required_transactions
        .iter()
        .map(|tx| tx.from)
        .collect::<HashSet<_>>();
    countries.extend(finance.required_transactions.iter().map(|tx| tx.to));
    
    let mut country_to_country_tx: HashMap<(&str, &str), f64> = HashMap::new();
    for tx in finance.required_transactions {
        let country_pair = (tx.from, tx.to);
        match country_to_country_tx.entry(country_pair) {
            Entry::Occupied(entry) => {

            }
            // Entry::
        }
    }

    let connection_count = countries.len() * (countries.len() - 1) / 2;
    for sub_count in 1..(connection_count + 1) {

    }
}
