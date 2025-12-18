/**
 * Sandbox script for testing code snippets.
 */
import { NS } from "@ns";

export async function main(ns: NS) {
    const info = ns.singularity.getAugmentationStats("ECorp HVMind Implant");
    ns.tprint(`ECorp HVMind Implant: ${JSON.stringify(info)}`);
}
