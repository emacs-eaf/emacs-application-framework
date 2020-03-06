// Trigger event
export function trigger(element, type) {
    if (!element || !type) {
        return;
    }

    // Create and dispatch the event
    const event = new Event(type);

    // Dispatch the event
    element.dispatchEvent(event);
}

export default {};
