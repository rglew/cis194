var gulp = require('gulp');
var shell = require('gulp-shell')

gulp.task('default', function() {
  gulp.start('test');
});

gulp.task('watch', function() {

  // Watch .hs files
  gulp.watch('*.hs', ['test']);

});

gulp.task('test', shell.task([
  'runhaskell Main.hs'
]))

