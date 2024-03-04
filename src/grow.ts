import { NS } from "@ns";

export async function main(ns: NS): Promise<void> {
    const target = (ns.args[0] as string) ?? "n00dles";
    const stock = (ns.args[1] as boolean) ?? false;

    for (; ;) {
        await ns.grow(target, { stock });
    }
}
