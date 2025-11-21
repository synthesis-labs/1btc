pub mod cli;
pub mod protocol;

use rand::{Rng, rngs::StdRng};

pub struct ShuffledSequenceGenerator {
    pool: Vec<u32>,
    next_value: u32,
    rng: StdRng,
}

impl ShuffledSequenceGenerator {
    pub fn new(pool_size: usize, rng: StdRng) -> Self {
        let pool: Vec<u32> = (0..pool_size as u32).collect();
        Self {
            pool,
            next_value: pool_size as u32,
            rng,
        }
    }

    pub fn next(&mut self) -> Option<u32> {
        if self.pool.is_empty() {
            // If pool is empty, just return none
            None
        } else {
            // Pick random index from pool
            let idx = self.rng.random_range(0..self.pool.len());
            let selected = self.pool[idx];
            
            // Replace with next sequential value
            self.pool[idx] = self.next_value;
            self.next_value += 1;
            
            Some(selected)
        }
    }
}
