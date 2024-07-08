function changeIframeSrc(newSrc) {
    // Create a switch to change the iframe source
    switch (newSrc) {
        case 'datasets':
            document.getElementById('myIframe').src = 'https://tracelac.shinyapps.io/datasets/';
            break;
        case 'divipola':
            document.getElementById('myIframe').src = 'https://tracelac.shinyapps.io/divipola/';
            break;
        case 'demograficos':
            document.getElementById('myIframe').src = 'https://tracelac.shinyapps.io/demograficos/';
            break;
        case 'geoespaciales':
            document.getElementById('myIframe').src = 'https://tracelac.shinyapps.io/geoespaciales/';
            break;
        case 'clima':
            document.getElementById('myIframe').src = 'https://tracelac.shinyapps.io/climate/'            ;
            break;
        case 'proypoblacionales':
            document.getElementById('myIframe').src = 'https://tracelac.shinyapps.io/population-projections/';
            break;
    }
}