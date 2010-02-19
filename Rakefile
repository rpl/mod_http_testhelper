require 'rake'
require 'rake/clean'

# Configuration
START_MODULE = "myerlang"
TEST_MODULE = "test_myerlang"
MNESIA_DIR = "/tmp"


# No Need to change
PWD = `pwd`.strip
INCLUDE = "include"
ERLC_FLAGS = "-I#{INCLUDE} +warn_unused_vars +warn_unused_import"

SRC = FileList['src/**/*.erl']
OBJ = SRC.pathmap("%{src,ebin}X.beam")
CLEAN.include(['**/*.dump'])
CLOBBER.include(['**/*.beam'])

directory 'ebin'


rule ".beam" =>  ["%{ebin,src}X.erl"] do |t|
  sh "erlc -pa ebin -W #{ERLC_FLAGS} -o ebin #{t.source}"
end

desc "Compile all"
task :compile do
  sh "erl -make"
end
### OLD task :compile => ['ebin'] + OBJ

desc "Open up a shell"
task :shell => [:compile] do
    sh("erl -sname #{START_MODULE} -pa #{PWD}/ebin")
end

desc "Open up a shell and run #{START_MODULE}:start()" 
task :run => [:compile] do
    sh("erl -sname #{START_MODULE} -pa #{PWD}/ebin -run #{START_MODULE} start")
end

def prepare_commontest
  mkdir_p "OUTPUT/test"
  mkdir_p "OUTPUT/ebin-codecov"
  sh("cp -rf src/*.erl ebin/* OUTPUT/ebin-codecov")
  commontest_options = <<-EOS
    -dir . -logdir OUTPUT/test -cover test/cover.spec -include $PWD/include 
  EOS
end

desc "Run Unit Tests" 
task :test => [:compile] do
  sh("erl -noshell -pa $PWD/OUTPUT/ebin-codecov -s ct_run script_start -s erlang halt #{prepare_commontest.chomp}")
end

desc "Run Unit Tests Step by Step" 
task :test_step => [:compile] do
  sh("erl -noshell  -pa $PWD/OUTPUT/ebin-codecov -s ct_run script_start -step #{prepare_commontest.chomp}")
end

task :test_vts => [:compile] do
  sh("erl -noshell  -pa $PWD/OUTPUT/ebin-codecov -s webtool script_start vts chromium-browser -s ct_run script_start #{prepare_commontest.chomp}")
end

desc "Generate Documentation"
task :doc do
    sh("cd doc && erl -noshell -s edoc_run files '[\"../#{SRC.join("\", \"../")}\"]' '[{includes, 'include'}]' -run init stop")
end


task :default => :compile
