(defproject neko "3.1.0-beta2"
  :description "Neko is a toolkit designed to make Android development using Clojure easier and more fun."
  :url "https://github.com/alexander-yakushev/neko"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure-android/clojure "1.5.1-jb"]]
  :source-paths ["src" "src/clojure"]
  :java-source-paths ["src/java" "gen"]

  :profiles {:default [:user :dev :android-common]}
  :plugins [[lein-droid "0.3.0-beta2"]]

  :android {:library true
            :target-sdk :ics})
