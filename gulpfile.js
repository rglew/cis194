var gulp = require('gulp');
var shell = require('gulp-shell')

gulp.task('default', function() {
  gulp.start('test');
});

gulp.task('watch', function() {

  // Watch .hs files
  gulp.watch('./src/*/*.hs', ['test']);

});

gulp.task('test', shell.task([
  'cd src && runhaskell Main.hs'
]));

gulp.task('w4', shell.task([
  'cd src && runhaskell Week4Tests.hs'
]));


