import { NS } from "@ns";

export async function main(ns: NS): Promise<void> {
    const target = (ns.args[0] as string) ?? "n00dles";
    for (; ;) {
        await ns.hack(target);
    }
}