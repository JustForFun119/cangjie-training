module.exports = {
	globDirectory: 'docs/',
	globPatterns: [
		'**/*.js'
	],
	globIgnores: [
		'js/cljs-runtime/*',
		'js/main.js'
	],
	swDest: 'docs/sw.js',
	ignoreURLParametersMatching: [
		/^utm_/,
		/^fbclid$/
	]
};