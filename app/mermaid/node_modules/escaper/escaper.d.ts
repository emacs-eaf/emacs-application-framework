/*!
 * Escaper
 * https://github.com/kobezzza/Escaper
 *
 * Released under the MIT license
 * https://github.com/kobezzza/Escaper/blob/master/LICENSE
 */

declare const Escaper: {
	VERSION: any[];
	cache: Record<string, string>;
	content: string[];
	snakeskinRgxp: RegExp | null;
	symbols: string | null;
	paste(str: string, content?: string[]);
	replace(str: string, withComments?: boolean, content?: string[], snakeskin?: boolean);
	replace(str: string, params: {
		'@label'?: string,
		'@all'?: boolean,
		'@comments'?: boolean,
		'@strings'?: boolean,
		'@literals'?: boolean,
		'`'?: boolean,
		"'"?: boolean,
		'"'?: boolean,
		'/'?: boolean,
		'//'?: boolean,
		'//*'?: boolean,
		'//!'?: boolean,
		'//#'?: boolean,
		'//@'?: boolean,
		'//$'?: boolean,
		'/*'?: boolean,
		'/**'?: boolean,
		'/*!'?: boolean,
		'/*#'?: boolean,
		'/*@'?: boolean,
		'/*$'?: boolean
	}, content?: string[], snakeskin?: boolean);
};

declare module 'escaper' {
	export = Escaper;
}
