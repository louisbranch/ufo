module.exports = function(grunt) {
  grunt.initConfig({
    regarde: {
      js: {
        files: ["src/**/*.erl", "test/**/*.erl"],
        tasks: ["shell:doc", "shell:dialyzer", "shell:test"]
      }
    },
    shell: {
      doc: {
        command: "rebar doc apps=ufo",
        options: {
          stdout: true,
          stderr: true
        }
      },
      dialyzer: {
        command: "dialyzer -r src --src --quiet",
        options: {
          stdout: true,
          stderr: true
        }
      },
      test: {
        command: "rebar eunit apps=ufo",
        options: {
          stdout: true,
          stderr: true
        }
      }
    }
  });

  // Tasks
  grunt.registerTask("default", "eunit test", function () {
    var tasks = ["regarde"];
    grunt.option("force", true);
    grunt.task.run(tasks);
  });
  grunt.registerTask("test", ["shell:test"]);

  // Load Dependencies
  grunt.loadNpmTasks("grunt-regarde");
  grunt.loadNpmTasks("grunt-shell");
  grunt.loadNpmTasks("grunt-notify");

};
