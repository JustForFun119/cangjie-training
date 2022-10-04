module.exports = {
	globDirectory: 'public/',
	globPatterns: [
		'**/*.{html,js}'
	],
	swDest: 'public/sw.js',
	ignoreURLParametersMatching: [
		/^utm_/,
		/^fbclid$/
	]
};