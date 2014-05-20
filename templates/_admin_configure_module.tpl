<div class="modal-body">

    <div class="control-group">
        <label class="control-label" for="apikey">{_ Github webhook token _}</label>
        <div class="controls">
            <input type="text" id="apikey" name="api_key" value="{{ m.config.mod_github.webhook_token.value|escape }}" class="span4 do_autofocus" />
            {% wire id="apikey" type="blur" action={config_toggle module="mod_github" key="webhook_token" } %}
        </div>
        <p class="info-block">{_ This token is used to secure the webhook API call for updating this website on a code push to Github (or similar services like BitBucket). _}</p>
        <p>{_ To run the webhook update, configure your service to POST to the following URL: _}
            <br/><b>http://{{ m.req.host }}/api/github/webhook?token=...</b></p>
            
    </div>

</div>

<div class="modal-footer">
    {% button class="btn" text=_"Close" action={dialog_close} tag="a" %}
</div>

