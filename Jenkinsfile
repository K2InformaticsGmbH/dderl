node ('master') {
  stage('Cleanup') {
    sh 'rm -rf *'
  }
  stage('Checkout') {
      checkout([$class: 'GitSCM',
                branches: [[name: '*/master']],
                doGenerateSubmoduleConfigurations: false,
                extensions: [],
                submoduleCfg: [],
                userRemoteConfigs: [[credentialsId: '2f075d3a-5c1e-4e60-ad74-208148e12e45',
                                     url: 'https://github.com/K2InformaticsGmbH/dderl']]
            ])
  }
  stage('Build') {
    env.INSTANT_CLIENT_LIB_PATH = "/usr/lib/oracle/12.1/client64/lib/"
    env.INSTANT_CLIENT_INCLUDE_PATH = "/usr/include/oracle/12.1/client64/"
    env.ERL_INTERFACE_DIR = "/usr/lib64/erlang/lib/erl_interface-3.8.2"
    sh 'rebar3 as prod release'
  }
  stage('Package') {
    sh 'rebar3 as prod erlpkg'
  }
  stage('Archive') {
    archiveArtifacts artifacts: '_build/prod/rel/erlpkg/**/*.rpm', onlyIfSuccessful: true
  }
}
node ('windows7_vm') {
  stage('Cleanup') {
    bat 'for /D %%p IN ("*.*") do rmdir "%%p" /s /q'
  }
  stage('Checkout') {
      checkout([$class: 'GitSCM',
                branches: [[name: '*/master']],
                doGenerateSubmoduleConfigurations: false,
                extensions: [],
                submoduleCfg: [],
                userRemoteConfigs: [[credentialsId: '2f075d3a-5c1e-4e60-ad74-208148e12e45',
                                     url: 'https://github.com/K2InformaticsGmbH/dderl']]
            ])
  }
  stage('Build') {
    env.INSTANT_CLIENT_LIB_PATH = "C:\\oracle\\instantclient\\instantclient_12_1"
    env.ERL_INTERFACE_DIR = "C:\\Program Files\\erlang\\erl7.0\\lib\\erl_interface-3.8"
    sh 'rebar3 as prod release'
  }
  stage('Package') {
    sh 'rebar3 as prod erlpkg'
  }  
  stage('Archive') {
    archiveArtifacts artifacts: 'rel/erlpkg_release/build/**/*.msi', onlyIfSuccessful: true
  }
}
