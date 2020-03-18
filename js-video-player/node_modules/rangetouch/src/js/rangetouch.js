// ==========================================================================
// rangetouch.js v2.0.0
// Making <input type="range"> work on touch devices
// https://github.com/sampotts/rangetouch
// License: The MIT License (MIT)
// ==========================================================================

import defaults from './config';
import { matches } from './utils/css';
import { trigger } from './utils/events';
import is from './utils/is';
import { round } from './utils/numbers';

class RangeTouch {
    /**
     * Setup a new instance
     * @param {String|Element} target
     * @param {Object} options
     */
    constructor(target, options) {
        if (is.element(target)) {
            // An Element is passed, use it directly
            this.element = target;
        } else if (is.string(target)) {
            // A CSS Selector is passed, fetch it from the DOM
            this.element = document.querySelector(target);
        }

        if (!is.element(this.element) || !is.empty(this.element.rangeTouch)) {
            return;
        }

        this.config = Object.assign({}, defaults, options);

        this.init();
    }

    static get enabled() {
        return 'ontouchstart' in document.documentElement;
    }

    /**
     * Setup multiple instances
     * @param {String|Element|NodeList|Array} target
     * @param {Object} options
     */
    static setup(target, options = {}) {
        let targets = null;

        if (is.empty(target) || is.string(target)) {
            targets = Array.from(document.querySelectorAll(is.string(target) ? target : 'input[type="range"]'));
        } else if (is.element(target)) {
            targets = [target];
        } else if (is.nodeList(target)) {
            targets = Array.from(target);
        } else if (is.array(target)) {
            targets = target.filter(is.element);
        }

        if (is.empty(targets)) {
            return null;
        }

        const config = Object.assign({}, defaults, options);

        if (is.string(target) && config.watch) {
            // Create an observer instance
            const observer = new MutationObserver(mutations => {
                Array.from(mutations).forEach(mutation => {
                    Array.from(mutation.addedNodes).forEach(node => {
                        if (!is.element(node) || !matches(node, target)) {
                            return;
                        }

                        // eslint-disable-next-line no-unused-vars
                        const range = new RangeTouch(node, config);
                    });
                });
            });

            // Pass in the target node, as well as the observer options
            observer.observe(document.body, {
                childList: true,
                subtree: true,
            });
        }

        return targets.map(t => new RangeTouch(t, options));
    }

    init() {
        // Bail if not a touch enabled device
        if (!RangeTouch.enabled) {
            return;
        }

        // Add useful CSS
        if (this.config.addCSS) {
            // TODO: Restore original values on destroy
            this.element.style.userSelect = 'none';
            this.element.style.webKitUserSelect = 'none';
            this.element.style.touchAction = 'manipulation';
        }

        this.listeners(true);

        this.element.rangeTouch = this;
    }

    destroy() {
        // Bail if not a touch enabled device
        if (!RangeTouch.enabled) {
            return;
        }

        this.listeners(false);

        this.element.rangeTouch = null;
    }

    listeners(toggle) {
        const method = toggle ? 'addEventListener' : 'removeEventListener';

        // Listen for events
        ['touchstart', 'touchmove', 'touchend'].forEach(type => {
            this.element[method](type, event => this.set(event), false);
        });
    }

    /**
     * Get the value based on touch position
     * @param {Event} event
     */
    get(event) {
        if (!RangeTouch.enabled || !is.event(event)) {
            return null;
        }

        const input = event.target;
        const touch = event.changedTouches[0];
        const min = parseFloat(input.getAttribute('min')) || 0;
        const max = parseFloat(input.getAttribute('max')) || 100;
        const step = parseFloat(input.getAttribute('step')) || 1;
        const delta = max - min;

        // Calculate percentage
        let percent;
        const clientRect = input.getBoundingClientRect();
        const thumbWidth = ((100 / clientRect.width) * (this.config.thumbWidth / 2)) / 100;

        // Determine left percentage
        percent = (100 / clientRect.width) * (touch.clientX - clientRect.left);

        // Don't allow outside bounds
        if (percent < 0) {
            percent = 0;
        } else if (percent > 100) {
            percent = 100;
        }

        // Factor in the thumb offset
        if (percent < 50) {
            percent -= (100 - percent * 2) * thumbWidth;
        } else if (percent > 50) {
            percent += (percent - 50) * 2 * thumbWidth;
        }

        // Find the closest step to the mouse position
        return min + round(delta * (percent / 100), step);
    }

    /**
     * Update range value based on position
     * @param {Event} event
     */
    set(event) {
        if (!RangeTouch.enabled || !is.event(event) || event.target.disabled) {
            return;
        }

        // Prevent text highlight on iOS
        event.preventDefault();

        // Set value
        event.target.value = this.get(event);

        // Trigger event
        trigger(event.target, event.type === 'touchend' ? 'change' : 'input');
    }
}

export default RangeTouch;
