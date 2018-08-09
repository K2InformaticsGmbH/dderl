import './swagger.css';
import 'swagger-ui/dist/swagger-ui.css';
import SwaggerUI from 'swagger-ui';
import './logo.png';

$(function () {
    $(document.body).html(require('./index.html'));
    $.get("brand.json", function (brand) {
        if ($.type(brand) === "string")
            brand = JSON.parse(brand);
        $('#brand_title').text(brand.title);
        $('#brand_banner').attr('href', brand.url);
        $('#brand_image').one('load', function () {
            $('#k2logo').css('height', $('#brand_image').height());
        });
        if (brand.logo) {
            $('#brand_image').attr('src', brand.logo);
        } else {
            $('#brand_image').remove();
        }

        SwaggerUI({
            dom_id: '#SwaggerUI',
            url: window.location.origin + brand.spec
        });

        if (versions && versions.swaggerUi && versions.swaggerUi.version) {
            $('#swagger_version').text(versions.swaggerUi.version);
        } else {
            console.info("Swagger-ui version is not available");
        }
    });
});
