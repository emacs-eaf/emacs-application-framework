// Get the number of decimal places
export function getDecimalPlaces(value) {
    const match = `${value}`.match(/(?:\.(\d+))?(?:[eE]([+-]?\d+))?$/);

    if (!match) {
        return 0;
    }

    return Math.max(
        0,
        // Number of digits right of decimal point.
        (match[1] ? match[1].length : 0) -
            // Adjust for scientific notation.
            (match[2] ? +match[2] : 0),
    );
}

// Round to the nearest step
export function round(number, step) {
    if (step < 1) {
        const places = getDecimalPlaces(step);
        return parseFloat(number.toFixed(places));
    }
    return Math.round(number / step) * step;
}

export default {};
