import { NS } from '@ns';

export async function main(ns: NS): Promise<void> {
    ns.ui.openTail();
    while (true) {
        await ns.sleep(5000);
    }
}
