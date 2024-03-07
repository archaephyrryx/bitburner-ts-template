import { NS } from '@ns';
import { bold, reset } from './helper';
import { formatRatio } from './helper';

export const BLADE_SNAP_FILE = "bladesnap.json.txt";

export interface SnapId {
    lastInstallTime: number,
}

export interface BladeSnap {
    id: SnapId,
    contracts: number,
    operations: number,
}

export function getSnapshot(ns: NS): BladeSnap {
    const snapshot = createSnapshot(ns);
    if (ns.fileExists(BLADE_SNAP_FILE, "home")) {
        const oldSnapshot = loadSnapshot(ns, true);

        if (snapshot.id.lastInstallTime == oldSnapshot.id.lastInstallTime) return oldSnapshot;
    }
    ns.write(BLADE_SNAP_FILE, JSON.stringify(snapshot), 'w');
    return snapshot;
}

function loadSnapshot(ns: NS, validate_id = true): BladeSnap {
    if (!ns.fileExists(BLADE_SNAP_FILE, "home")) {
        saveSnapshot(ns);
    }
    const data = JSON.parse(ns.read(BLADE_SNAP_FILE));
    if ((!validate_id || (data.id !== undefined && Number.isFinite(data.id.lastInstallTime))) &&
        Number.isSafeInteger(data.contracts) &&
        Number.isSafeInteger(data.operations)) {
        return data as BladeSnap;
    }
    throw new Error(`Snapshot ${data} is malformed!`);
}

export function saveSnapshot(ns: NS) {
    const snapshot = createSnapshot(ns);
    if (ns.fileExists(BLADE_SNAP_FILE, "home")) {
        const oldSnapshot = loadSnapshot(ns, true);

        if (snapshot.id.lastInstallTime == oldSnapshot.id.lastInstallTime) return;
    }
    ns.write(BLADE_SNAP_FILE, JSON.stringify(snapshot), 'w');
}

function createSnapshot(ns: NS): BladeSnap {
    const id = { lastInstallTime: ns.getResetInfo().lastAugReset };
    const [contracts, operations] = getBladeSuccesses(ns);
    return { id, contracts, operations };
}

export function getBladeSuccesses(ns: NS): [number, number] {
    let opCount = 0;
    for (const op of ns.bladeburner.getOperationNames()) {
        opCount += ns.bladeburner.getActionSuccesses("Operations", op);
    }

    let contCount = 0;
    for (const cont of ns.bladeburner.getContractNames()) {
        contCount += ns.bladeburner.getActionSuccesses("Contracts", cont);
    }
    return [contCount, opCount];
}

export function successesSinceInstall(ns: NS, snapshot: BladeSnap) {
    const [contracts, ops] = getBladeSuccesses(ns);
    return [contracts - snapshot.contracts, ops - snapshot.operations];
}

export async function main(ns: NS) {
    const snapshot = getSnapshot(ns);
    ns.tprint(`${bold}INFO: ${showBladeSnap(ns, snapshot)}${reset}`);
}

export function showBladeSnap(ns: NS, snapshot: BladeSnap): string {
    const [contracts, ops] = successesSinceInstall(ns, snapshot);
    return formatSuccesses(contracts, ops);
}

export function formatSuccesses(contracts: number, ops: number) {
    return `${contracts} Contracts, ${ops} Operations (ratio: ${formatRatio(contracts, ops)}) since last reset.`;
}
