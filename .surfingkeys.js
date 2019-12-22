// url config
var musicUrls = Array(
    "https://www.youtube.com/watch?v=wL5z9cQfK5U", // I see fire
    "https://www.youtube.com/watch?v=dq32Qo2v1LU", // Should it matter
    "https://www.youtube.com/watch?v=nSDgHBxUbVQ",
)

var urls = Array(
    "http://lambda-the-ultimate.org/node?from=30",
    "https://www.inoreader.com/all_articles",
    "https://techsingular.net/page/3/",
    "https://stackoverflow.com/questions/tagged/c?sort=frequent&page=2&pagesize=15",
    "https://stackoverflow.com/questions/tagged/c++?sort=frequent&pageSize=15",
    "https://stackoverflow.com/questions/tagged/python?sort=frequent&pageSize=15",
    "https://stackoverflow.com/questions/tagged/operating-system?sort=frequent&page=4&pagesize=15",
    "https://stackoverflow.com/questions/tagged/scheme?sort=frequent&pagesize=15",
    "https://github.com/",
    "http://aosabook.org/en/index.html",
    "https://www.zhihu.com/people/skywind3000/activities",
)


// ************************************************** //
// ************************************************** //
// ***********************UTILS********************** //
// ************************************************** //
// ************************************************** //
function addQuickSearch(key, description, url) {
    addSearchAliasX(key, description, url);
    mapkey('s'+key, "#8Search" + description, function () {
        Front.openOmnibar({type: "SearchEngine", extra: key});
    })
}

function addQuickSearchBasedGoogle(key, keyword) {
    url = "https://www.google.com/search?q=" + keyword + " "
    description = "Google " + keyword
    addQuickSearch(key, description, url)
}

function runtimeMapkey(key, keywords, command) {
    mapkey(key, keywords, function () {
        RUNTIME(command)
    });
}

function newtabMapkey(key, keywords, url) {
    mapkey(key, keywords, function () {
        tabOpenLink(url);
    });
}


// ************************************************** //
// ************************************************** //
// *******************NAVIGATION********************* //
// ************************************************** //
// ************************************************** //
unmap('e');
unmap('d');
mapkey('<Ctrl-u>', '#2Scroll a page up', function() {
    Normal.scroll("pageUp")
}, {repeatIgnore: true});
mapkey('<Ctrl-d>', '#2Scroll a page down', function() {
    Normal.scroll("pageDown")
}, {repeatIgnore: true});

unmap('S');
unmap('D');
mapkey('<Ctrl-o>', '#4Go back in history', function () {
    history.go(-1)
}, {repeatIgnore: true});
mapkey('<Ctrl-i>', '#4Go forward in history', function () {
    history.go(1)
}, {repeatIgnore: true});

unmap('af');
unmap('cf');
unmap('C');
mapkey('F', '#1Open a link in new tab', function () {
    Hints.create("", Hints.dispatchMouseClick, {tabbed: true, active: false})
});
mapkey('<Alt-f>', '#1Open multiple links in a new tab', function () {
    Hints.create("", Hints.dispatchMouseClick, {tabbed: true, active: false, multipleHits: true})
});




// ************************************************** //
// ************************************************** //
// **********************OMNIBAR********************* //
// ************************************************** //
// ************************************************** //
unmap('O');
unmap('ox');
unmap('og');
unmap('ob');
unmap('oh');
unmap('om');
unmap('go');
mapkey('U', '#8Open recently closed URL', function () {
    Front.openOmnibar({type: "URLs", extra: "getRecentlyClosed"})
});
mapkey('O', '#8Open Search with alias g', function () {
    Front.openOmnibar({type: "SearchEngine", extra: "g"})
});
mapkey('H', '#8Open URL from history', function () {
    Front.openOmnibar({type: "History"})
});
mapkey('M', '#8Open URL from vim-like marks', function () {
    Front.openOmnibar({type: "VIMarks"})
});
mapkey('o', '#8Open a URL in current tab', function () {
    Front.openOmnibar({type: "SearchEngine", extra: "g", tabbed: false})
});
// Bookmarks
mapkey('e', '#8Open first bookmark folder', function () {
    Normal.feedkeys('b');
    Normal.feedkeys('Enter');
});
mapkey('A', '#8add page to  first bookmark folder', function () {
    Normal.feedkeys('ab');
    Normal.feedkeys('Enter');
});




// ************************************************** //
// ************************************************** //
// **********************QUICK SEARCH**************** //
// ************************************************** //
// ************************************************** //
removeSearchAliasX('d');  // duckduckgo
removeSearchAliasX('b');  // baidu
removeSearchAliasX('w');  // bing
removeSearchAliasX('s');  // stackoverflow
removeSearchAliasX('h');  // github
addQuickSearch('j', 'en => zh-CN', 'https://www.google.com/search?q=what is ');
addQuickSearch('f', 'zh-CN => en', 'https://translate.google.cn/?safe=strict&um=1&ie=UTF-8&hl=zh-CN&client=tw-ob#auto/en/');
addQuickSearch('t', 'Taobao', 'https://s.taobao.com/search?q={0}&imgfile=&commend=all&ssid=s5-e&search_type=item&sourceId=tb.index&spm=a21bo.50862.201856-taobao-item.1&ie=utf8&initiative_id=tbindexz_20170610');
addQuickSearch('c', 'GitHub code', 'https://github.com/search?type=Code&utf8=%E2%9C%93&q=');
addQuickSearch('e', 'Chrome Store', 'https://chrome.google.com/webstore/search/');
// addQuickSearch('g', 'Google Scholar', 'https://scholar.google.com/scholar?hl=en&as_sdt=0%2C5&q={0}&btnG=');
addQuickSearch('m', 'Music', 'https://y.qq.com/portal/search.html#page=1&searchid=1&remoteplace=txt.yqq.top&t=song&w=');

addQuickSearchBasedGoogle('a', "Archlinux")
addQuickSearchBasedGoogle('b', "Sublime Text")
addQuickSearchBasedGoogle('d', 'douban')
addQuickSearchBasedGoogle('D', 'difference')
addQuickSearchBasedGoogle('s', 'stackoverflow')
addQuickSearchBasedGoogle('z', 'zhihu')
addQuickSearchBasedGoogle('y', 'Youtube')
addQuickSearchBasedGoogle('l', 'AZLyrics')
addQuickSearchBasedGoogle('v', 'v2ex')
addQuickSearchBasedGoogle('w', 'Wikipedia')
addQuickSearchBasedGoogle('g', 'GitHub')
addQuickSearchBasedGoogle('G', 'goodreaders')
// addQuickSearchBasedGoogle('p', 'pdf')
addQuickSearchBasedGoogle('p', 'pypi')
addQuickSearchBasedGoogle('r', 'ROS')
addQuickSearchBasedGoogle('q', 'Qt')




// ************************************************** //
// ************************************************** //
// **********************MISC************************ //
// ************************************************** //
// ************************************************** //
unmap(';j');
runtimeMapkey('cd', '#12Close Downloads Shelf', "closeDownloadsShelf")

// shortcut for pandora.com
mapkey('a', '#7pandora again', function () {
    document.getElementsByClassName("TunerControl ReplayButton Tuner__Control__Button Tuner__Control__Replay__Button")[0].click();
}, {domain: /pandora\.com/i});
mapkey('l', '#7pandora next song', function () {
    document.getElementsByClassName("TunerControl SkipButton Tuner__Control__Button Tuner__Control__Skip__Button")[0].click();
}, {domain: /pandora\.com/i});
mapkey('u', '#7pandora nice song', function () {
    document.getElementsByClassName("TunerControl ThumbUpButton Tuner__Control__Button Tuner__Control__ThumbUp__Button")[0].click();
}, {domain: /pandora\.com/i});
mapkey('d', '#7pandora bad song', function () {
    document.getElementsByClassName("TunerControl ThumbDownButton Tuner__Control__Button Tuner__Control__ThumbDown__Button")[0].click();
}, {domain: /pandora\.com/i});

// shortcut for Music.com
mapkey('l', '#7QQ Music prev song', function () {
    document.getElementsByClassName("btn_big_next")[0].click();
}, {domain: /qq\.com/i});
mapkey('h', '#7QQ Music prev song', function () {
    document.getElementsByClassName("btn_big_prev")[0].click();
}, {domain: /qq\.com/i});



// ************************************************** //
// ************************************************** //
// *******************SETTING************************ //
// ************************************************** //
// ************************************************** //
settings.newTabPosition = 'right';
// settings.showModeStatus  = true;
// settings.showProxyInStatusBar = true;
settings.smartPageBoundary = false;
settings.clickableSelector = "*.jfk-button, *.goog-flat-menu-button";
Hints.characters = "asdfcvwelion";




// ************************************************** //
// ************************************************** //
// **********************Tabs************************ //
// ************************************************** //
// ************************************************** //
unmap('E');
map('<Ctrl-0>', 'g$');
map('<Alt-e>', 'g$');
runtimeMapkey('<Ctrl-h>', '#3Go one tab left', "previousTab");
runtimeMapkey('<Ctrl-l>', '#3Go one tab right', "nextTab");
unmap('X');
runtimeMapkey('u', '#3Restore closed tab', "openLast");
mapkey('J', '#3Choose a tab', function() {
    Front.chooseTab()
});
unmap('x');
unmap('gxt');
unmap('gxT');
unmap('gx0');
unmap('gx$');
runtimeMapkey('ch', '#3Close tabs on left', "closeTabLeft");
runtimeMapkey('cl', '#3Close tabs on right', "closeTabRight");
runtimeMapkey('cH', '#3Close all tabs on left', "closeTabsToLeft");
runtimeMapkey('cL', '#3Close all tabs on right', "closeTabsToRight");

unmap('T');
unmap('t');
mapkey('tl', '#3Open resting things', function () {
    var url = urls[Math.floor(Math.random()*urls.length)]
    tabOpenLink(url);
});

newtabMapkey('tn', '#3Open nba top 10', 'https://www.youtube.com/user/NBA/videos')
newtabMapkey('tg', '#3Open github', 'https://www.github.com')
mapkey('tm', '#3Open random music', function () {
    var urls = musicUrls
    var url = urls[Math.floor(Math.random()*urls.length)]
    tabOpenLink(url);
});




// ************************************************** //
// ************************************************** //
// *********************Control********************** //
// ************************************************** //
// ************************************************** //
function isNegativeURL(str) {
    return (new RegExp('cc.163|'+
                       'zhanqi.tv|'+
                       'douyu.com|'+
                       'huya.com|'+
                       'hupu.com|'+
                       'ruby-china.org|'+
                       // 'emacs-china.org|'+
                       'v2ex.com|'+
                       'web.sanguosha.com|'+
                       'zhihu.com'
                      )).test(str);
}

function isOkURL(str) {
    return (new RegExp('(webcache)|'+
                       '(v2ex.com\\/t)|'+
                       '(search)'
                      )).test(str);
}

onloadFunc = {
    "jumpInitCommit": function() {
        var aTags = document.getElementsByTagName("a");
        var searchText = "Older";

        for (var i = 0; i < aTags.length; i++) {
            if (aTags[i].textContent == searchText) {
                window.location.replace(aTags[i].href);
                break;
            }
        }
    },
    "rescuetime": function() {
        var urls = musicUrls
        var url = window.location.href;
        if(isNegativeURL(url) && !isOkURL(url)) {
            var url = urls[Math.floor(Math.random()*urls.length)]
            window.location.replace(url);
        }
    },
}

window.onload = function() {
    // onloadFunc["jumpInitCommit"]()
    // onloadFunc["rescuetime"]()
}
