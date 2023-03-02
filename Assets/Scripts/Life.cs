using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;
public class Life : MonoBehaviour
{
    public Text lives;
    void Update()
    {
        lives.text = Stats.life.ToString();
    }
}
