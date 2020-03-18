/*!
 * Escaper
 * https://github.com/kobezzza/Escaper
 *
 * Released under the MIT license
 * https://github.com/kobezzza/Escaper/blob/master/LICENSE
 */

/** @const */
var Escaper = {
	/** @type {!Array} */
	VERSION: [],

	/** @type {!Object} */
	cache: [],

	/** @type {!Array} */
	content: [],

	/** @type {?string} */
	symbols: null,

	/** @type {RegExp} */
	snakeskinRgxp: null,

	/**
	 * @param {string} str
	 * @param {(Object<string, boolean>|boolean)=} [opt_withCommentsOrParams]
	 * @param {Array=} [opt_content]
	 * @param {?boolean=} [opt_snakeskin]
	 * @return {string}
	 */
	replace: function (str, opt_withCommentsOrParams, opt_content, opt_snakeskin) {},

	/**
	 * @param {string} str
	 * @param {Array=} [opt_content]
	 * @return {string}
	 */
	paste: function (str, opt_content) {}
};
