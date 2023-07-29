import { NS, NetscriptPort } from "@ns";
import { nodes, ServicePort } from "global";
import { canHack } from "helper";

export async function broadcastTarget(ns: NS, port: NetscriptPort, target: string): Promise<void> {
    while (!port.tryWrite(target)) {
        await ns.sleep(100);
    }
}
