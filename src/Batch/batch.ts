import { NS, Server } from '@ns'
import { canHack } from '/helper';

export type BatchConfig = {
    skimRatio: number,
    targetServer: string,
};

const ErrLogFile = `Batch/error.log.txt`;

type OutputStream = 'toast' | 'terminal' | 'tail';

function tee(ns: NS, message: string, dest: OutputStream = 'tail') {
    const logMessage = `${Date.now().toString()}: ${ns.getScriptName()} (${ns.pid}): ${message}`;
    ns.write(ErrLogFile, logMessage + '\n', "a");
    switch (dest) {
        case 'terminal':
            ns.tprint(message);
            break;
        case 'toast':
            ns.toast(logMessage, 'error', 3000);
            break;
        case 'tail':
            ns.print(message);
            break;
    }
}

export type BatchResult = false | BatchInfo;

export type BatchInfo = {
    hackPid: number,
    growPid: number,
    weakenPid0: number,
    weakenPid1?: number,
}

type Validity = {
    valid: true;
} | {
    valid: false;
    reason: string;
};

export function isValidTarget(ns: NS, targetServer: string): Validity {
    if (!ns.serverExists(targetServer)) {
        return { valid: false, reason: "does not exist" };
    }
    const server = ns.getServer(targetServer);
    if (server.purchasedByPlayer || server.hostname == "home") {
        return { valid: false, reason: "is owned by player" };
    }
    if (!canHack(ns, targetServer, 1)) {
        return { valid: false, reason: "cannot be hacked" };
    }
    if (server.moneyMax !== undefined && server.moneyMax > 0) {
        return { valid: true };
    } else {
        return { valid: false, reason: "does not have money" };
    }
}

export async function prepServer(ns: NS, targetServer: string): Promise<boolean> {
    const validity = isValidTarget(ns, targetServer);
    if (!validity.valid) {
        tee(ns, `targetServer ${targetServer} ${validity.reason}`);
        return false;
    }
    const server = ns.getServer(targetServer);
    const player = ns.getPlayer();

    const serverAtMin: Server = { ...server, hackDifficulty: server.minDifficulty };

    if (server.moneyMax === undefined) {
        tee(ns, `targetServer ${targetServer} moneyMax is undefined`);
        return false;
    }

    const moneyMax = server.moneyMax;

    const growThreadsCurrent = ns.formulas.hacking.growThreads(server, player, moneyMax);
    throw new Error('unimplemented');
}

export async function launchBatch(ns: NS, conf: BatchConfig): Promise<BatchResult> {
    if (!ns.serverExists(conf.targetServer)) {
        tee(ns, `targetServer ${conf.targetServer} does not exist`);
        return false;
    } else if (!canHack(ns, conf.targetServer, 1)) {
        tee(ns, `targetServer ${conf.targetServer} cannot be hacked`);
        return false;
    }
    const server = ns.getServer(conf.targetServer);
    const player = ns.getPlayer();

    const hackTime = ns.formulas.hacking.hackTime(server, player);
    throw new Error('unimplemented');
}
