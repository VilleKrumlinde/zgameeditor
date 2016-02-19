# WebOpen {#WebOpen}

Component to access web pages. This can be used for posting and displaying online high scores or making turn based multiplayer games.

Example usage: @ref YakYakReader sample project.

## Properties

@dl

@dt Url
@dd The URL of the web-page. For instance:@n"http://www.zgameeditor.org/?action=search&q=overview".

@dt ResultString
@dd This string holds the content of the web page.

@dt InBrowser
@dd Set this property to true if you want the web-page to open in a separate window.

@dlx

## List Properties

@dl

@dt OnResult
@dd This will be executed when the web page has been read (only when InBrowser=false). Put code here to read ResultString.

@dlx
