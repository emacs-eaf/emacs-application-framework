// ==========================================================================
// Type checking utils
// ==========================================================================

const getConstructor = input => (input !== null && typeof input !== 'undefined' ? input.constructor : null);
const instanceOf = (input, constructor) => Boolean(input && constructor && input instanceof constructor);

const isNullOrUndefined = input => input === null || typeof input === 'undefined';
const isObject = input => getConstructor(input) === Object;
const isNumber = input => getConstructor(input) === Number && !Number.isNaN(input);
const isString = input => getConstructor(input) === String;
const isBoolean = input => getConstructor(input) === Boolean;
const isFunction = input => getConstructor(input) === Function;
const isArray = input => Array.isArray(input);
const isNodeList = input => instanceOf(input, NodeList);
const isElement = input => instanceOf(input, Element);
const isEvent = input => instanceOf(input, Event);
const isEmpty = input =>
    isNullOrUndefined(input) ||
    ((isString(input) || isArray(input) || isNodeList(input)) && !input.length) ||
    (isObject(input) && !Object.keys(input).length);

export default {
    nullOrUndefined: isNullOrUndefined,
    object: isObject,
    number: isNumber,
    string: isString,
    boolean: isBoolean,
    function: isFunction,
    array: isArray,
    nodeList: isNodeList,
    element: isElement,
    event: isEvent,
    empty: isEmpty,
};
