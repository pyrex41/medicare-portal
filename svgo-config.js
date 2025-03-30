module.exports = {
  plugins: [
    'removeDimensions',
    'removeViewBox',
    'cleanupIDs',
    'removeRasterImages',
    'removeUselessDefs',
    'removeNonInheritableGroupAttrs',
    'removeUselessStrokeAndFill',
    'convertStyleToAttrs',
    'removeDuplicateElements',
    {
      name: 'removeAttrs',
      params: {
        attrs: ['class', 'data-name', 'fill-rule', 'clip-rule']
      }
    },
    {
      name: 'convertColors',
      params: {
        currentColor: false,
        names2hex: true,
        rgb2hex: true,
        shorthex: true
      }
    },
    {
      name: 'convertPathData',
      params: {
        noSpaceAfterFlags: false
      }
    },
    {
      name: 'cleanupNumericValues',
      params: {
        floatPrecision: 2,
        leadingZero: true,
        defaultPx: true,
        convertToPx: true
      }
    }
  ]
} 