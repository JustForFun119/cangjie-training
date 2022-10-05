module.exports = {
	globDirectory: 'public/',
	globPatterns: [
		'**/*.{html,js}'
	],
	globIgnores: [
		'js/cljs-runtime/*'
	],
	swDest: 'public/sw.js',
	ignoreURLParametersMatching: [
		/^utm_/,
		/^fbclid$/
	]
};