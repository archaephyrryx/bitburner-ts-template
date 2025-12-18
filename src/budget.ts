import { NS, ScriptArg, AutocompleteData } from "@ns";
import { arrayEq } from "./util/arraytools";
import { printWaitingMoney } from "./helper";

type Id = number;

type BudgetItem = { readonly id: Id, readonly amount: number, readonly pid: number, readonly verb?: string };

export class BudgetSemaphore {
    private items: BudgetItem[];
    private next_id: Id;

    constructor() {
        this.items = [];
        this.next_id = 0;
    }

    public getTotal(): number {
        const ret = this.items.reduce((acc, { amount }) => acc + amount, 0);
        return ret;
    }

    public request(ns: NS, amount: number, verb?: string): Id {
        const id = this.next_id++;
        this.items.push({ id, amount, pid: ns.pid, verb });
        return id;
    }

    public getVerb(ns: NS, id: number): string | undefined {
        const item = this.items.find((item) => item.id === id);
        return item?.verb;
    }

    public until(id: Id, forceOrder = true): [number, number] {
        let before = 0;
        let at = 0;
        const nItems = this.items.length;
        for (let i = 0; i < nItems; i++) {
            const item = this.items[i];
            if (item.id < id && forceOrder) {
                before += item.amount;
            }
            if (item.id === id) {
                at = this.items[i].amount;
                break;
            }
        }
        return [before, at];
    }

    public remove(ns: NS, id: Id): boolean {
        const tmp = this.items.filter(({ id: val }) => id !== val);
        const removed = (tmp.length === this.items.length - 1);
        if (removed) {
            this.items = tmp;
        } else {
            ns.tprint(`ERROR: Request to remove nonexistent (or already removed) budget item ${id}`);
        }
        return removed;
    }

    public audit(ns: NS): BudgetItem[] {
        for (const item of this.items) {
            if (!ns.isRunning(item.pid)) {
                this.remove(ns, item.id);
            }
        }
        const ret = [...this.items];
        return ret;
    }

    public clearAll(): void {
        this.items = [];
        return;
    }
}

export const BUDGET = new BudgetSemaphore();

export async function reserveMoney(ns: NS, amount: number, verb?: string): Promise<Id> {
    if (amount < 0) {
        throw new Error(`${ns.getScriptName()}: reserveMoney: amount must be greater than zero`);
    }
    const id = await BUDGET.request(ns, amount, verb);
    return id;
}

export async function makePurchase<T>(ns: NS, id: Id, f: ((ns: NS) => Promise<T>), respectOrder = true): Promise<T> {
    ns.disableLog('getServerMoneyAvailable');
    for (; ;) {
        const [before, exact] = BUDGET.until(id, respectOrder);
        const baseVerb = BUDGET.getVerb(ns, id) ?? `fulfill #${id}`;
        const verb = respectOrder ? `${baseVerb} (reserving $${ns.formatNumber(before)})` : baseVerb;
        const targetAmount = respectOrder ? before + exact : exact;
        const currentAmount = ns.getServerMoneyAvailable("home");
        printWaitingMoney(ns, currentAmount, targetAmount, verb);
        if (currentAmount >= targetAmount) {
            const ret = await f(ns);
            BUDGET.remove(ns, id);
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
    ns.ui.openTail();
    let snapshot: BudgetItem[] = [];
    if (flags.purge as ScriptArg as boolean === true) {
        BUDGET.clearAll();
    }
    for (; ;) {
        const current = BUDGET.audit(ns);
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

export function getAvailMoney(ns: NS): [number, number] {
    ns.disableLog('getServerMoneyAvailable');
    const total = ns.getServerMoneyAvailable("home");
    const reserved = BUDGET.getTotal();
    return [total, Math.max(total - reserved, 0)];
}
