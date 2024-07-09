function changeIframeSrc(newSrc) {
    // Create a switch to change the iframe source
    switch (newSrc) {
        case 'datasets':
            document.getElementById('myIframe').src = 'https://tracelac.uniandes.edu.co:3838/shiny-apps/colopendata/climate';
            break;
        case 'divipola':
            document.getElementById('myIframe').src = 'https://tracelac.uniandes.edu.co:3838/shiny-apps/colopendata/divipola';
            break;
        case 'demograficos':
            document.getElementById('myIframe').src = 'https://tracelac.uniandes.edu.co:3838/shiny-apps/colopendata/download-demographic';
            break;
        case 'geoespaciales':
            document.getElementById('myIframe').src = 'https://tracelac.uniandes.edu.co:3838/shiny-apps/colopendata/download-geospatial';
            break;
        case 'clima':
            document.getElementById('myIframe').src = 'https://tracelac.uniandes.edu.co:3838/shiny-apps/colopendata/list-datasets';
            break;
        case 'proypoblacionales':
            document.getElementById('myIframe').src = 'https://tracelac.uniandes.edu.co:3838/shiny-apps/colopendata/population-projections';
            break;
    }
}
