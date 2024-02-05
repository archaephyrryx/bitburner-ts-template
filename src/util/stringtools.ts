export function mimic(model: string, pattern = '='): string {
    if (model.length % pattern.length !== 0) {
        throw new Error(`cannot mimic with pattern ${pattern} that does not cleanly divide into model length ${model.length}`);
    }
    const times = (model.length / pattern.length);
    return pattern.repeat(times);
}

export function mimicPad(model: string, s: string, pattern = ' '): string {
    if ((model.length - s.length) % pattern.length !== 0) {
        throw new Error(`cannot mimic with pattern '${pattern}' that does not cleanly divide into padding length ${model.length - s.length}`);
    }
    const times = ((model.length - s.length) / pattern.length);
    return (s + pattern.repeat(times));
}
