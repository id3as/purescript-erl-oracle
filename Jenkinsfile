pipeline {
  agent any

  stages {
    stage('Nix Build') {
      steps {
        sh '''. ~/.nix-profile/etc/profile.d/nix.sh
              nix-shell --run exit | tee ./nix-build.log'''
      }
    }

    stage('Tests') {
      steps {
        sh '''. ~/.nix-profile/etc/profile.d/nix.sh
        # can't run test because it needs oci creds
        nix-shell --command "make all"'''
      }
    }
  }

  post {
    success {
      discordSend description: "Build Completed", link: env.BUILD_URL, result: currentBuild.currentResult, showChangeset: true, title: JOB_NAME, webhookURL: "${env.DISCORD_ENDPOINT}"
    }

    unstable {
      script {
        discordSend description: "Build Completed (Unstable)", link: env.BUILD_URL, result: currentBuild.currentResult, showChangeset: true, title: JOB_NAME, webhookURL: "${env.DISCORD_ENDPOINT}"
      }
    }

    failure {
      script {
        discordSend description: "Build Failed", link: env.BUILD_URL, result: currentBuild.currentResult, showChangeset: true, title: JOB_NAME, webhookURL: "${env.DISCORD_ENDPOINT}"
      }
    }
  }
}

