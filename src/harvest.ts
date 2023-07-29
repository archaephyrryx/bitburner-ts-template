import { NS, NetscriptPort, Player, Server } from "@ns";
import { ServicePort, nodes } from 'global';
import { canHack } from 'helper';

const serverObjects: { [key: string]: Server } = {};

function getServerObject(ns: NS, node: string): Server {
    if (ns.serverExists(node)) {
        if (!(serverObjects[node] ?? false)) {
            serverObjects[node] = ns.getServer(node);
        }
        return serverObjects[node];
    }
    throw new Error(`Server ${node} does not exist`);
}

const MoneyThresholdRatio = 0.75 as const;
const SecurityThresholdOffset = 5 as const;


type Monitor = {
    timePort: NetscriptPort,
    labelPort: NetscriptPort,
}

export async function hackOnce(ns: NS, server: Server, player: Player, monitor: Monitor) {
    const maxMoney = server.moneyMax ?? 0;
    if (maxMoney == 0) {
        return;
    }
    const minSecurity = server.minDifficulty ?? -1;
    if (minSecurity == -1) {
        return;
    }

    const moneyThresh = maxMoney * MoneyThresholdRatio;
    const securityThresh = minSecurity + SecurityThresholdOffset;

    let hasHacked = false;
    while (!hasHacked) {
        if (ns.getServerSecurityLevel(server.hostname) > securityThresh) {
            const label = `${server.hostname}.weaken`;
            const time = (ns.fileExists("Formulas.exe", "home") ? ns.formulas.hacking.weakenTime(server, player) : ns.getWeakenTime(server.hostname));
            monitor.timePort.write(time);
            monitor.labelPort.write(label);
            await ns.weaken(server.hostname);
        } else if (ns.getServerMoneyAvailable(server.hostname) < moneyThresh) {
            const label = `${server.hostname}.grow`;
            const time = (ns.fileExists("Formulas.exe", "home") ? ns.formulas.hacking.growTime(server, player) : ns.getGrowTime(server.hostname));
            monitor.timePort.write(time);
            monitor.labelPort.write(label);
            await ns.grow(server.hostname);
        } else {
            // Otherwise, hack it
            const label = `${server.hostname}.hack`;
            const time = (ns.fileExists("Formulas.exe", "home") ? ns.formulas.hacking.hackTime(server, player) : ns.getHackTime(server.hostname));
            monitor.timePort.write(time);
            monitor.labelPort.write(label);
            await ns.hack(server.hostname);
            hasHacked = true;
        }
    }
}

async function seasons(ns: NS): Promise<void> {
    const timePort = ns.getPortHandle(ServicePort.HarvestTimer);
    const labelPort = ns.getPortHandle(ServicePort.HarvestTimerMessage);
    const monitor = { timePort, labelPort };

    async function season(server: Server, player: Player) {
        if (canHack(ns, server.hostname)) {
            await hackOnce(ns, server, player, monitor);
            ns.toast(`Finished season on ${server.hostname}`, "success", 5000);
        }
    }

    for (; ;) {
        const player = ns.getPlayer();
        for (const node of nodes) {
            if (canHack(ns, node.name)) {
                await season(getServerObject(ns, node.name), player);
            }
        }
    }
}

export async function reap(ns: NS): Promise<number> {
    let accum = 0;
    for (const node of nodes) {
        if (canHack(ns, node.name)) {
            if (ns.getServerMoneyAvailable(node.name) > 0) {
                accum += (await ns.hack(node.name));
            }
        }
    }
    return accum;
}

export async function replant(ns: NS): Promise<void> {
    for (const node of nodes) {
        if (canHack(ns, node.name)) {
            await ns.grow(node.name);
            await ns.weaken(node.name);
        }
    }
}

export async function main(ns: NS): Promise<void> {
    switch (ns.args[0] ?? "reap") {
        case "reap": {
            const totalHarvest = await reap(ns);
            ns.toast(`Harvested $${totalHarvest} in total`, "success", 5000);
            return;
        }
        case "replant": {
            for (; ;) {
                await replant(ns);
            }
        }
        case "seasons": {
            await seasons(ns);
        }
    }
}