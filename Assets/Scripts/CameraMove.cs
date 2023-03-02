
using UnityEngine;

public class CameraMove : MonoBehaviour
{
    public float speed;
    public float scrollSpeed;
    private float _border;
    [SerializeField]private float minY;
    [SerializeField]private float maxY;

    private void Update()
    {
        MoveCamera();
        ZoomCamera();
    }
    
    void MoveCamera()
    {
        if (Gamemanager.gameOver)
        {
            this.enabled = false;
            return;
        }
        if (Input.GetKey("w") || Input.mousePosition.y >= Screen.height - _border)
        {
            transform.Translate(Vector3.forward * speed * Time.deltaTime, Space.World);
        }
        if (Input.GetKey("s") || Input.mousePosition.y <= _border)
        {
            transform.Translate(-Vector3.forward * speed * Time.deltaTime, Space.World);
        }
        if (Input.GetKey("d") || Input.mousePosition.x >= Screen.width - _border)
        {
            transform.Translate(Vector3.right * speed * Time.deltaTime, Space.World);
        }
        if (Input.GetKey("a") || Input.mousePosition.x <= _border)
        {
            transform.Translate(-Vector3.right * speed * Time.deltaTime, Space.World);
        }
    }
    void ZoomCamera()
    {
        if (Gamemanager.gameOver)
        {
            this.enabled = false;
            return;
        }
        float scroll = Input.GetAxis("Mouse ScrollWheel");
        Vector3 pos = transform.position;
        pos.y -= scroll * 500 *scrollSpeed * Time.deltaTime;
        pos.y = Mathf.Clamp(pos.y, minY, maxY);
        transform.position = pos;
    }
}
