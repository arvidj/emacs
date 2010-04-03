#!/bin/bash

cd plugins/

svn_repos=( "http://js2-mode.googlecode.com/svn/trunk js2"
    "http://geben-on-emacs.googlecode.com/svn/trunk geben"
    "http://yasnippet.googlecode.com/svn/trunk yasnippet"
    )

hg_repos=( "https://emacs-soap-client.googlecode.com/hg/ emacs-soap-client" )


case $1 in
    add )
        echo "adding"

        # len=${#svn_repos[@]}
        # for (( i = 0 ; i < len ; i++ ))  
        # do
        #     svn co ${svn_repos[$i]}
        # done

        len=${#hg_repos[@]}
        for (( i = 0 ; i < len ; i++ ))  
        do
            hg clone ${hg_repos[$i]}
        done
        ;;
    update )
        echo "updating" 

        len=${#svn_repos[@]}
        for (( i = 0 ; i < len ; i++ ))  
        do
            name=`echo ${svn_repos[$i]} | cut -d " " -f 2`
            cd $name
            svn up
            cd ..
        done

        len=${#hg_repos[@]}
        for (( i = 0 ; i < len ; i++ ))  
        do
            name=`echo ${hg_repos[$i]} | cut -d " " -f 2`
            
            cd $name
            hg pull
            hg update
            cd ..
        done

        ;;
esac