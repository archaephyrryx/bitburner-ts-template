import { NS } from '@ns';

interface RNG {
    random(): number;
}

class RNG0 implements RNG {
    x: number;
    m = 1024;
    a = 341;
    c = 1;

    constructor() {
        this.x = 0;
        this.reset();
    }

    step(): void {
        this.x = (this.a * this.x + this.c) % this.m;
    }

    random(): number {
        this.step();
        return this.x / this.m;
    }

    reset(): void {
        this.x = new Date().getTime() % this.m;
    }
}

export const BAD_RNG_PERIOD = 1024;

export async function recordRandom(ns: NS): Promise<void> {
    const rng = new RNG0();
    const buffer = [];

    for (let i = 0; i < BAD_RNG_PERIOD; i++) {
        buffer.push(`${i}: ${rng.random() < 0.5 ? 'H' : 'T'}`);
    }
    ns.print(buffer.join('\n'));
    return;
}

export async function main(ns: NS) {
    ns.singularity.softReset("lgg.js");
}
