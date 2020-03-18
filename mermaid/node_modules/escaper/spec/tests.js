/*!
 * Escaper
 * https://github.com/kobezzza/Escaper
 *
 * Released under the MIT license
 * https://github.com/kobezzza/Escaper/blob/master/LICENSE
 */

describe('Escaper', function () {
	it('should work with " ... "', function () {
		var str = Escaper.replace('Hello "friend\\\""!');

		expect(str)
			.toBe('Hello __ESCAPER_QUOT__0_!');

		expect(Escaper.paste(str))
			.toBe('Hello "friend\\\""!');

		var str2 = Escaper.replace('Hello "friend\\\""!');

		expect(str2)
			.toBe('Hello __ESCAPER_QUOT__0_!');

		expect(Escaper.paste(str2))
			.toBe('Hello "friend\\\""!');

		var stack = [];
		var str3 = Escaper.replace('Hello "friend\\\""!', false, stack);

		expect(str3)
			.toBe('Hello __ESCAPER_QUOT__0_!');

		expect(Escaper.paste(str3, stack))
			.toBe('Hello "friend\\\""!');

		stack = [];
		var str4 = Escaper.replace('Hello "friend\\\""!', {'"': -1}, stack);

		expect(str4)
			.toBe('Hello !');

		expect(Escaper.paste(str4, stack))
			.toBe('Hello !');
	});

	it("should work with ' ... '", function () {
		var str = Escaper.replace("Hello 'friend\\\''!");

		expect(str)
			.toBe('Hello __ESCAPER_QUOT__1_!');

		expect(Escaper.paste(str))
			.toBe("Hello 'friend\\\''!");

		var stack = [];
		var str2 = Escaper.replace("Hello 'friend\\\''!", false, stack);

		expect(str2)
			.toBe('Hello __ESCAPER_QUOT__0_!');

		expect(Escaper.paste(str2, stack))
			.toBe("Hello 'friend\\\''!");
	});

	it('should work with ` ... `', function () {
		var stack = [];
		var str = Escaper.replace('Hello `friend`!', false, stack);

		expect(str)
			.toBe('Hello __ESCAPER_QUOT__0_!');

		expect(Escaper.paste(str, stack))
			.toBe("Hello `friend`!");

		var str2 = Escaper.replace('Hello `friend${1 + {foo: {}} + `foo` + /1/}`!', false, stack);

		expect(str2)
			.toBe('Hello __ESCAPER_QUOT__1_1 + {foo: {}} + __ESCAPER_QUOT__2_ + __ESCAPER_QUOT__3___ESCAPER_QUOT__4_!');

		expect(Escaper.paste(str2, stack))
			.toBe("Hello `friend${1 + {foo: {}} + `foo` + /1/}`!");

		var str3 = Escaper.replace('Hello `friend\\${foo}`!', false, stack);

		expect(str3)
			.toBe('Hello __ESCAPER_QUOT__5_!');

		expect(Escaper.paste(str3, stack))
			.toBe('Hello `friend\\${foo}`!');

		var str4 = Escaper.replace('Hello `friend${foo/* fooo */}`!', true, stack);

		expect(str4)
			.toBe('Hello __ESCAPER_QUOT__6_foo__ESCAPER_QUOT__7___ESCAPER_QUOT__8_!');

		expect(Escaper.paste(str4, stack))
			.toBe('Hello `friend${foo/* fooo */}`!');
	});

	it("should work with / ... /", function () {
		var stack = [];
		var str = Escaper.replace("Hello + /friend\\//gmi!", false, stack);

		expect(str)
			.toBe('Hello + __ESCAPER_QUOT__0_!');

		expect(Escaper.paste(str, stack))
			.toBe('Hello + /friend\\//gmi!');

		var str2 = Escaper.replace("Hello, /friend\\/[//.]/gmi!", false, stack);

		expect(str2)
			.toBe('Hello, __ESCAPER_QUOT__1_!');

		expect(Escaper.paste(str2, stack))
			.toBe('Hello, /friend\\/[//.]/gmi!');

		var str3 = Escaper.replace('/friend\\/[//.]/gmi!, /friend\\/[//.]/gmi', false, stack);

		expect(str3)
			.toBe('__ESCAPER_QUOT__2_!, __ESCAPER_QUOT__3_');

		expect(Escaper.paste(str3, stack))
			.toBe('/friend\\/[//.]/gmi!, /friend\\/[//.]/gmi');
	});

	it("should work with / ... / (advanced test)", function () {
		var stack = [];
		var str = Escaper.replace('2 >> /foo/ < /bar/ ^ /car/ [/bar/] foo typeof /mu/ /mu/', true, stack);

		expect(str)
			.toBe('2 >> __ESCAPER_QUOT__0_ < __ESCAPER_QUOT__1_ ^ __ESCAPER_QUOT__2_ [__ESCAPER_QUOT__3_] foo typeof __ESCAPER_QUOT__4_ /mu/');

		expect(Escaper.paste(str, stack))
			.toBe('2 >> /foo/ < /bar/ ^ /car/ [/bar/] foo typeof /mu/ /mu/');
	});

	it("should work with single-line comments", function () {
		var stack = [];
		var str = Escaper.replace(
			("Hello // the comment\
\n			Friend!"), true, stack);

		expect(str)
			.toBe('Hello __ESCAPER_QUOT__0_\n\t\t\tFriend!');

		expect(Escaper.paste(str, stack))
			.toBe('Hello // the comment\n\t\t\tFriend!');
	});

	it("should work with //!", function () {
		var stack = [];
		var str = Escaper.replace(
			("Hello // the comment //! fffuuu\
\n//! fffuuuu\
\n			Friend!"), {'//!': true}, stack);

		expect(str)
			.toBe('Hello // the comment //! fffuuu\n__ESCAPER_QUOT__0_\n\t\t\tFriend!');

		expect(Escaper.paste(str, stack))
			.toBe('Hello // the comment //! fffuuu\n//! fffuuuu\n\t\t\tFriend!');
	});

	it("should work with multiline comments", function () {
		var stack = [];
		var str = Escaper.replace('Hello /*/ the comment */ Friend!', true, stack);

		expect(str)
			.toBe('Hello __ESCAPER_QUOT__0_ Friend!');

		expect(Escaper.paste(str, stack))
			.toBe('Hello /*/ the comment */ Friend!');
	});

	it("should work with Snakeskin", function () {
		var stack = [];
		var str = Escaper.replace('foo|replace /hello/g|join "world"', true, stack, true);

		expect(str)
			.toBe('foo|replace __ESCAPER_QUOT__0_|join __ESCAPER_QUOT__1_');

		expect(Escaper.paste(str, stack))
			.toBe('foo|replace /hello/g|join "world"');
	});

	it("should work with custom parameters", function () {
		var stack = [];
		var str = Escaper.replace('"Hello" /* the comment */ + /Friend/gim /** foo */!', {
			'"': true,
			'/': true,
			'/*': true
		}, stack);

		expect(str)
			.toBe('__ESCAPER_QUOT__0_ __ESCAPER_QUOT__1_ + __ESCAPER_QUOT__2_ /** foo */!');

		expect(Escaper.paste(str, stack))
			.toBe('"Hello" /* the comment */ + /Friend/gim /** foo */!');
	});

	it("should work with deep literals", function () {
		var stack = [];
		var str = Escaper.replace('"Hello" /** "foo" */', {'"': true}, stack);

		expect(str)
			.toBe('__ESCAPER_QUOT__0_ /** "foo" */');

		expect(Escaper.paste(str, stack))
			.toBe('"Hello" /** "foo" */');
	});

	it("should work with @all", function () {
		var stack = [];
		var str = Escaper.replace('"Hello" /* the comment */ + /Friend/gim /** foo */!', {'@all': true, '/*': -1}, stack);

		expect(str)
			.toBe('__ESCAPER_QUOT__0_  + __ESCAPER_QUOT__1_ __ESCAPER_QUOT__2_!');

		expect(Escaper.paste(str, stack))
			.toBe('"Hello"  + /Friend/gim /** foo */!');
	});

	it("should work with @comments", function () {
		var stack = [];
		var str = Escaper.replace('"Hello" /* the comment */ + /Friend/gim /** foo */!', {'@comments': -1}, stack);

		expect(str)
			.toBe('"Hello"  + /Friend/gim !');

		expect(Escaper.paste(str, stack))
			.toBe('"Hello"  + /Friend/gim !');
	});

	it("should work with @comments, @literals and @all", function () {
		var stack = [];
		var str = Escaper.replace('"Hello" /* the comment */ + /Friend/gim /** foo */!', {
			'@all': -1,
			'@comments': false,
			'@literals': true
		}, stack);

		expect(str)
			.toBe('__ESCAPER_QUOT__0_ /* the comment */ + __ESCAPER_QUOT__1_ /** foo */!');

		expect(Escaper.paste(str, stack))
			.toBe('"Hello" /* the comment */ + /Friend/gim /** foo */!');
	});
});
