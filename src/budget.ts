import { NS, ScriptArg, AutocompleteData } from "@ns";
import { arrayEq } from "./util/arraytools";

type Id = number;

type BudgetItem = { readonly id: Id, readonly amount: number, readonly pid: number };

export class BudgetSemaphore {
    private items: BudgetItem[];
    private next_id: Id;
    private lock_pid: number;

    constructor() {
        this.items = [];
        this.next_id = 0;
        this.lock_pid = 0;
    }

    public async getTotal(ns: NS): Promise<number> {
        await this.getLock(ns);
        const ret = this.items.reduce((acc, { amount }) => acc + amount, 0);
        this.freeLock(ns);
        return ret;
    }

    protected async getLock(ns: NS) {
        while (this.lock_pid != 0 && this.lock_pid != ns.pid && ns.isRunning(this.lock_pid)) {
            await ns.sleep(100);
        }
        this.lock_pid = ns.pid;
    }

    protected freeLock(ns: NS) {
        if (ns.pid == this.lock_pid || this.lock_pid == 0 || !ns.isRunning(this.lock_pid)) {
            this.lock_pid = 0;
            return;
        }
        throw new Error(`Lock ${(this.lock_pid == 0 ? 'not held' : `held by ${this.lock_pid}`)} but release requested by ${ns.pid}`);
    }

    /**
     * Async-safe (hopefully) request submission for an unspecified
     * budget item of a specified monetary amount.
     */
    public async request(ns: NS, amount: number): Promise<Id> {
        await this.getLock(ns);
        const id = this.next_id++;
        this.items.push({ id, amount, pid: ns.pid });
        this.freeLock(ns);
        return id;
    }

    public async until(ns: NS, id: Id): Promise<number> {
        await this.getLock(ns);
        let sum = 0;
        const nItems = this.items.length;
        for (let i = 0; i < nItems; i++) {
            sum += this.items[i].amount;
            if (this.items[i].id === id) {
                break;
            }
        }
        this.freeLock(ns);
        return sum;
    }

    public async remove(ns: NS, id: Id): Promise<boolean> {
        await this.getLock(ns);
        const tmp = this.items.filter(({ id: val }) => id !== val);
        const removed = (tmp.length === this.items.length - 1);
        if (removed) {
            this.items = tmp;
        } else {
            ns.tprint(`ERROR: Request to remove nonexistent (or already removed) budget item ${id}`);
        }
        this.freeLock(ns);
        return removed;
    }

    public async audit(ns: NS): Promise<BudgetItem[]> {
        await this.getLock(ns);
        for (const item of this.items) {
            if (!ns.isRunning(item.pid)) {
                this.remove(ns, item.id);
            }
        }
        const ret = [...this.items];
        this.freeLock(ns);
        return ret;
    }

    public async clearAll(ns: NS): Promise<void> {
        await this.getLock(ns);
        this.items = [];
        this.freeLock(ns);
        return;
    }
}

export const BUDGET = new BudgetSemaphore();

export async function reserveMoney(ns: NS, amount: number): Promise<Id> {
    if (amount < 0) {
        throw new Error(`${ns.getScriptName()}: reserveMoney: amount must be greater than zero`);
    }
    const id = await BUDGET.request(ns, amount);
    return id;
}

export async function makePurchase<T>(ns: NS, id: Id, f: ((ns: NS) => Promise<T>)): Promise<T> {
    for (; ;) {
        const targetAmount = await BUDGET.until(ns, id);
        if (ns.getServerMoneyAvailable("home") >= targetAmount) {
            const ret = await f(ns);
            await BUDGET.remove(ns, id);
            return ret;
        }
        await ns.sleep(1000);
    }
}

function showBudget(ns: NS, items: BudgetItem[]) {
    ns.print("=== Budget Overview ===");
    if (items.length > 0) {
        for (const item of items) {
            ns.print(`\t> $${ns.formatNumber(item.amount)} for #${item.id}`);
        }
    } else {
        ns.print(`\t> NO ITEMS`)
    }
    ns.print("=======================\n");
}

export async function main(ns: NS) {
    ns.disableLog('sleep');
    const flags = ns.flags([
        ["purge", false]
    ]);
    ns.tail();
    let snapshot: BudgetItem[] = [];
    if (flags.purge as ScriptArg as boolean === true) {
        await BUDGET.clearAll(ns);
    }
    for (; ;) {
        const current = await BUDGET.audit(ns);
        if (!arrayEq(current, snapshot)) {
            showBudget(ns, current);
            snapshot = current;
        }
        await ns.sleep(1000);
    }
}

export function autocomplete(data: AutocompleteData, args: string[]) {
    data.flags([['purge', false]]);
    return [];
}
